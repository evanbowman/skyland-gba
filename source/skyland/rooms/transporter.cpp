////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "transporter.hpp"
#include "globals.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/recoverCharacterScene.hpp"
#include "skyland/scene/transportCharacterScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(transporter_reload_ms);



void Transporter::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_transporter)->c_str();
}



Transporter::Transporter(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Transporter::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (recharge_ > 0) {

        Room::ready();

        recharge_ -= delta;

        if (recharge_ < 0) {
            recharge_ = 0;

            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            }

            if (parent()->interior_visible()) {
                schedule_repaint();
            }
        }
    }
}



void Transporter::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

    if (recharge_ <= 0) {
        // Fully recharged.
    } else if (recharge_ < 1000 * transporter_reload_ms) {
        recharge_ += delta;
    }
}



void Transporter::___rewind___finished_reload(Platform& pfrm, App& app)
{
    recharge_ = 1;

    if (parent()->interior_visible()) {
        parent()->repaint(pfrm, app);
    }
}



void Transporter::___rewind___ability_used(Platform& pfrm, App& app)
{
    recharge_ = 0;

    if (parent()->interior_visible()) {
        parent()->repaint(pfrm, app);
    }
}



void Transporter::recover_character(Platform& pfrm,
                                    App& app,
                                    const RoomCoord& position)
{
    begin_recharge();

    if (parent()->interior_visible()) {
        schedule_repaint();
    }

    auto island = other_island(app);
    if (island == nullptr) {
        return;
    }

    Room::ready();

    if (auto room = island->get_room(position)) {
        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {

            bool owned_character_in_slot = false;
            for (auto& chr : room->characters()) {
                if (chr->owner() not_eq &island->owner() and
                    chr->grid_position() == position) {
                    owned_character_in_slot = true;
                }
            }

            if ((not owned_character_in_slot or
                 (owned_character_in_slot and
                  (*it)->owner() not_eq &island->owner())) and
                (*it)->grid_position() == position) {
                auto unlinked = std::move(*it);
                room->characters().erase(it);

                // If the character was in the process of moving, we need to
                // detach its path, as we are transporting the character back to
                // our island, where the path would make no sense.
                unlinked->drop_movement_path();

                const RoomCoord dst = {this->position().x,
                                       u8(this->position().y + 1)};

                network::packet::ChrDisembarkV2 packet;
                packet.chr_id_.set((*unlinked).id());
                packet.dst_x_ = dst.x;
                packet.dst_y_ = dst.y;
                packet.transporter_x_ = dst.x;
                packet.transporter_y_ = dst.x;
                packet.transporter_near_ = &parent()->owner() == &app.player();
                network::transmit(pfrm, packet);

                // Maybe you're thinking: why is he recording two separate
                // events? Wouldn't it be better to just record a single type of
                // event and handle them all the same way? If we had tons of
                // memory, I wouldn't have needed to split the network events
                // and the timestream events. But for rewinding we care about
                // the previous state, and for multiplayer, we care about the
                // current state, and storing them both in the same structure
                // uses a lot more memory (limiting the amount of stuff that
                // we're able to rewind). Furthermore, our six-byte network
                // packets can't fit the extra data. The code looks a bit
                // redundant, but the player will never see the code, but he/she
                // definitely might care about the amount of stuff in a level
                // that he/she can rewind through, so the decision here is
                // fairly straightforward.
                time_stream::event::CharacterDisembark e;
                e.id_.set(unlinked->id());
                e.previous_x_ = unlinked->grid_position().x;
                e.previous_y_ = unlinked->grid_position().y;
                e.chr_near_ = unlinked->parent() == &app.player_island();
                app.time_stream().push(app.level_timer(), e);

                // Again, the character is warping to a new location, let's
                // update its position.
                unlinked->set_grid_position(dst);

                unlinked->set_parent(parent());
                unlinked->transported();

                characters().push(std::move(unlinked));
                ready();
                return;
            } else {
                ++it;
            }
        }
    }
}



void Transporter::transport_occupant(Platform& pfrm,
                                     App& app,
                                     std::optional<RoomCoord> destination)
{
    begin_recharge();

    ready();

    if (parent()->interior_visible()) {
        schedule_repaint();
    }

    auto chr = characters().begin();

    auto island = other_island(app);
    if (island == nullptr) {
        return;
    }

    std::optional<RoomCoord> dest;

    if (not destination) {
        bool matrix[16][16];
        island->plot_walkable_zones(app, matrix);

        Buffer<RoomCoord, 32> slots;

        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 0; y < 16; ++y) {
                if (matrix[x][y]) {
                    slots.push_back({x, y});
                }
            }
        }

        while (not slots.empty()) {
            const auto index = rng::choice(slots.size(), rng::critical_state);
            auto slot = &slots[index];

            auto existing = island->character_at_location(*slot);
            if (existing and existing->owner() == &parent()->owner()) {
                // Do not transport into a slot containing one of your own
                // characters.
                slots.erase(slot);
            } else {
                dest = *slot;
                break;
            }
        }
    } else {
        dest = destination;
    }

    if (not dest) {
        // TODO: raise an alert, that there is no room to teleport into.
        return;
    }

    if (auto room = island->get_room(*dest)) {

        network::packet::ChrBoardedV2 packet;
        packet.chr_id_.set((*chr)->id());
        packet.dst_x_ = dest->x;
        packet.dst_y_ = dest->y;
        packet.transporter_x_ = (*chr)->grid_position().x;
        packet.transporter_y_ = (*chr)->grid_position().y;
        packet.transporter_near_ = &parent()->owner() == &app.player();
        network::transmit(pfrm, packet);

        time_stream::event::CharacterTransported e;
        e.previous_x_ = (*chr)->grid_position().x;
        e.previous_y_ = (*chr)->grid_position().y;
        e.id_.set((*chr)->id());
        e.source_near_ = &parent()->owner() == &app.player();
        app.time_stream().push(app.level_timer(), e);

        (*chr)->set_grid_position(*dest);
        (*chr)->set_parent(island);
        (*chr)->transported();

        // While it isn't really humanly possible to move a character out of a
        // transporter and then click the transporter to transport the occupant
        // before the occupant moves out of the transporter, the command-module
        // AI block is able to do this sometimes, hence we really do need to
        // throw out a character's movement path, as it technically could have a
        // path assigned.
        (*chr)->drop_movement_path();

        room->characters().push(std::move(*chr));
        room->ready();
    } else {
        return;
    }

    characters().erase(characters().begin());

    if (not pfrm.speaker().is_sound_playing("transporter")) {
        pfrm.speaker().play_sound("transporter", 2);
    }
}



ScenePtr<Scene>
Transporter::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    if (auto new_scene = Room::select(pfrm, app, cursor)) {
        return new_scene;
    }

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;


    if (mt_prep_seconds) {
        return null_scene();
    }


    if (not other_island(app)) {
        return null_scene();
    }


    if (recharge_) {
        return null_scene();
    } else if (length(characters())) {

        auto chr = characters().begin();
        if ((*chr)->has_movement_path()) {
            return null_scene();
        }

        if (parent()->has_radar() and parent() == &app.player_island()) {
            return scene_pool::alloc<TransportCharacterScene>(pfrm, position());
        } else {
            transport_occupant(pfrm, app);
        }

        return null_scene();
    } else {
        return scene_pool::alloc<RecoverCharacterScene>(position());
    }
}


void Transporter::render_interior(App* app, TileId buffer[16][16])
{
    if (recharge_) {
        buffer[position().x][position().y] = InteriorTile::transporter_recharge;
    } else {
        buffer[position().x][position().y] = InteriorTile::transporter_1;
    }
    buffer[position().x][position().y + 1] = InteriorTile::transporter_2;
}



void Transporter::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



bool Transporter::ready() const
{
    return recharge_ == 0 and
           parent()->power_supply() > parent()->power_drain();
}



// TODO: use this function above. No need for transport code to be
// duplicated across the codebase.
void transport_character_impl(App& app,
                              Island* src_island,
                              Island* dst_island,
                              CharacterId chr_id,
                              const RoomCoord& dst)
{
    auto found = src_island->find_character_by_id(chr_id);

    if (not found.first) {
        return;
    }

    if (auto room = found.second) {



        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {

            if ((*it)->id() == chr_id) {

                auto unlinked = std::move(*it);
                room->characters().erase(it);

                unlinked->set_grid_position(dst);
                unlinked->set_idle(app);
                unlinked->drop_movement_path();
                unlinked->set_parent(dst_island);
                unlinked->transported();

                if (auto dst_room = dst_island->get_room(dst)) {
                    dst_room->characters().push(std::move(unlinked));
                    dst_room->ready();
                }

                return;

            } else {
                ++it;
            }
        }
    }
}



void Transporter::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (health() <= 0) {
        ExploSpawner::create(pfrm, app, center());
    }
}



} // namespace skyland
