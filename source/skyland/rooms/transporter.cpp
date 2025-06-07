////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "transporter.hpp"
#include "globals.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/recoverCharacterScene.hpp"
#include "skyland/scene/transportCharacterScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(transporter_reload_ms);
SHARED_VARIABLE(transporter_goblin_perk_ms);



void Transporter::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_transporter)->c_str();
}



Transporter::Transporter(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Transporter::update(Time delta)
{
    Room::update(delta);

    if (recharge_ > 0) {

        Room::ready();

        if (is_powered_down()) {
            return;
        }

        recharge_ -= delta;

        if (recharge_ < 0) {
            recharge_ = 0;

            update_description();

            if (is_player_island(parent())) {
                time_stream::event::PlayerRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                APP.time_stream().push(APP.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                APP.time_stream().push(APP.level_timer(), e);
            }

            if (parent()->interior_visible()) {
                schedule_repaint();
            }
        }
    }
}



void Transporter::rewind(Time delta)
{
    Room::rewind(delta);

    if (is_powered_down()) {
        return;
    }

    if (recharge_ <= 0) {
        // Fully recharged.
    } else if (recharge_ < recharge_time()) {
        recharge_ += delta;
    }
}



void Transporter::___rewind___finished_reload()
{
    recharge_ = 1;

    if (parent()->interior_visible()) {
        parent()->repaint();
    }
}



void Transporter::___rewind___ability_used()
{
    recharge_ = 0;

    if (parent()->interior_visible()) {
        parent()->repaint();
    }
}



class CharacterOutline : public Entity
{
public:
    CharacterOutline(const Character& chr) : Entity({})
    {
        if (chr.sprite().get_flip().x) {
            sprite_.set_texture_index(122);
        } else {
            sprite_.set_texture_index(121);
        }

        // sprite_.set_position(chr.sprite().get_position());
        sprite_.set_origin(chr.sprite().get_origin());
        sprite_.set_size(Sprite::Size::w16_h32);

        auto o = chr.parent()->visual_origin();
        o.x += Fixnum::from_integer(chr.grid_position().x * 16);
        o.y += Fixnum::from_integer(chr.grid_position().y * 16 - 3);

        sprite_.set_position(o);
        position_ = o;
    }


    void update(Time delta) override
    {
        timer_ += delta;

        static const auto duration = milliseconds(200);

        if (timer_ > duration - duration / 4) {
            sprite_.set_alpha(Sprite::Alpha::translucent);
        }

        if (timer_ > duration) {
            kill();
            return;
        }

        const s16 shrink_amount = interpolate(128, 0, Float(timer_) / duration);

        sprite_.set_scale({shrink_amount, shrink_amount});

        auto pos = position_;
        pos.y += Fixnum::from_integer(shrink_amount >> 4);

        sprite_.set_position(pos);
    }

    void rewind(Time delta) override
    {
        kill();
    }

private:
    Time timer_ = milliseconds(16);
    Vec2<Fixnum> position_;
};



void Transporter::recover_character(const RoomCoord& position)
{
    begin_recharge();

    if (parent()->interior_visible()) {
        schedule_repaint();
    }

    auto island = other_island();
    if (island == nullptr) {
        return;
    }

    Room::ready();

    update_description();

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
                room->edit_characters().erase(it);

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
                packet.transporter_near_ = &parent()->owner() == &APP.player();
                network::transmit(packet);

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
                e.chr_near_ = is_player_island(unlinked->parent());
                APP.time_stream().push(APP.level_timer(), e);

                // Again, the character is warping to a new location, let's
                // update its position.
                unlinked->set_grid_position(dst);

                unlinked->set_parent(parent());
                unlinked->transported();

                make_transport_effect(*unlinked);

                edit_characters().push(std::move(unlinked));
                ready();
                return;
            } else {
                ++it;
            }
        }
    }
}



void make_transport_effect(Character& chr)
{
    if (chr.parent() == APP.opponent_island() and
        not chr.parent()->interior_visible()) {
        // Don't waste cpu on an effect that can't be seen.
    } else if (auto e = alloc_entity<CharacterOutline>(chr)) {
        APP.effects().push(std::move(e));
    }
}



void Transporter::transport_occupant(Optional<RoomCoord> destination)
{
    if (characters().empty()) {
        return;
    }

    begin_recharge();

    ready();

    update_description();

    if (parent()->interior_visible()) {
        schedule_repaint();
    }

    auto chr = characters().begin();

    auto island = other_island();
    if (island == nullptr) {
        return;
    }

    Optional<RoomCoord> dest;

    if (not destination) {
        bool matrix[16][16];
        island->plot_walkable_zones(matrix, chr->get());

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
        packet.transporter_near_ = &parent()->owner() == &APP.player();
        network::transmit(packet);

        time_stream::event::CharacterTransported e;
        e.previous_x_ = (*chr)->grid_position().x;
        e.previous_y_ = (*chr)->grid_position().y;
        e.id_.set((*chr)->id());
        e.source_near_ = &parent()->owner() == &APP.player();
        APP.time_stream().push(APP.level_timer(), e);

        (*chr)->set_grid_position(*dest);
        (*chr)->set_parent(island);
        (*chr)->transported();
        (*chr)->unpin();

        // While it isn't really humanly possible to move a character out of a
        // transporter and then click the transporter to transport the occupant
        // before the occupant moves out of the transporter, the command-module
        // AI block is able to do this sometimes, hence we really do need to
        // throw out a character's movement path, as it technically could have a
        // path assigned.
        (*chr)->drop_movement_path();

        if (auto ent = alloc_entity<CharacterOutline>(**chr)) {
            APP.effects().push(std::move(ent));
        }

        room->edit_characters().push(std::move(*chr));
        room->ready();
    } else {
        return;
    }

    edit_characters().erase(characters().begin());

    if (not PLATFORM.speaker().is_sound_playing("transporter")) {
        PLATFORM.speaker().play_sound("transporter", 2);
    }
}



ScenePtr Transporter::select_impl(const RoomCoord& cursor)
{
    if (auto new_scene = Room::select_impl(cursor)) {
        return new_scene;
    }

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;


    if (is_powered_down()) {
        return null_scene();
    }


    if (mt_prep_seconds) {
        return null_scene();
    }


    if (not other_island()) {
        return null_scene();
    }


    if (auto scn = reject_if_friendly()) {
        return scn;
    }

    if (recharge_) {
        return null_scene();
    } else if (length(characters())) {

        auto chr = characters().begin();
        if ((*chr)->has_movement_path()) {
            return null_scene();
        }

        if (parent()->has_radar() and is_player_island(parent())) {
            return make_scene<TransportCharacterScene>(position());
        } else {
            transport_occupant();
        }

        return null_scene();
    } else {
        if (is_player_island(parent())) {
            return make_scene<RecoverCharacterScene>(position());
        } else {
            PLATFORM.speaker().play_sound("beep_error", 3);
            return null_scene();
        }
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



void Transporter::on_powerchange()
{
    begin_recharge();
}



bool Transporter::allows_powerdown()
{
    return true;
}



bool Transporter::ready() const
{
    return recharge_ == 0 and
           parent()->power_supply() > parent()->power_drain();
}



// TODO: use this function above. No need for transport code to be
// duplicated across the codebase.
void transport_character_impl(Island* src_island,
                              Island* dst_island,
                              CharacterId chr_id,
                              const RoomCoord& dst)
{
    auto found = src_island->find_character_by_id(chr_id);


    if (not found.first) {
        return;
    }

    if (auto room = found.second) {

        room->update_description();

        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {

            if ((*it)->id() == chr_id) {

                auto unlinked = std::move(*it);
                room->edit_characters().erase(it);

                unlinked->set_grid_position(dst);
                unlinked->set_idle();
                unlinked->drop_movement_path();
                unlinked->set_parent(dst_island);
                unlinked->transported();

                if (auto dst_room = dst_island->get_room(dst)) {
                    dst_room->edit_characters().push(std::move(unlinked));
                    dst_room->ready();
                }

                return;

            } else {
                ++it;
            }
        }
    }
}



void Transporter::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



Time Transporter::recharge_time() const
{
    Time time = 1000 * transporter_reload_ms;
    if (is_player_island(parent()) and APP.faction() == Faction::goblin) {
        time -= 1000 * transporter_goblin_perk_ms;
    }
    if (amplify_) {
        time /= 2;
    }
    return time;
}



void Transporter::begin_recharge()
{
    recharge_ = recharge_time();
}



void Transporter::amplify(bool enable)
{
    amplify_ = enable;

    if (enable) {
        recharge_ = std::min(recharge_, recharge_time());
    }
}



} // namespace skyland
