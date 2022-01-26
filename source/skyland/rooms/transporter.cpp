#include "transporter.hpp"
#include "globals.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/recoverCharacterScene.hpp"
#include "skyland/scene/transportCharacterScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



SHARED_VARIABLE(transporter_reload_ms);



void Transporter::format_description(StringBuffer<512>& buffer)
{
    buffer += "Transport your crew into an enemy castle! Selects a random "
              "destination in the enemy fortress, unless you have a radar. "
              "Requires a workshop to build.";
}



Transporter::Transporter(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Transporter::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (recharge_ > 0) {
        recharge_ -= delta;

        if (recharge_ < 0) {
            recharge_ = 0;

            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(pfrm, app.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(pfrm, app.level_timer(), e);
            }

            if (parent()->interior_visible()) {
                parent()->repaint(pfrm, app);
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
                                    const Vec2<u8>& position)
{
    recharge_ = 1000 * transporter_reload_ms;

    if (parent()->interior_visible()) {
        parent()->repaint(pfrm, app);
    }

    auto island = other_island(app);
    if (island == nullptr) {
        return;
    }

    if (auto room = island->get_room(position)) {
        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {
            if ((*it)->owner() not_eq &island->owner() and
                (*it)->grid_position() == position) {
                auto unlinked = std::move(*it);
                room->characters().erase(it);

                // If the character was in the process of moving, we need to
                // detach its path, as we are transporting the character back to
                // our island, where the path would make no sense.
                unlinked->drop_movement_path();

                const Vec2<u8> dst = {this->position().x,
                                      u8(this->position().y + 1)};

                if (&parent()->owner() == &app.player()) {
                    network::packet::CharacterDisembark packet;
                    packet.src_x_ = unlinked->grid_position().x;
                    packet.src_y_ = unlinked->grid_position().y;
                    packet.dst_x_ = dst.x;
                    packet.dst_y_ = dst.y;
                    network::transmit(pfrm, packet);
                }

                time_stream::event::CharacterDisembark e;
                e.id_.set(unlinked->id());
                e.previous_x_ = unlinked->grid_position().x;
                e.previous_y_ = unlinked->grid_position().y;
                e.chr_near_ = unlinked->parent() == &app.player_island();
                app.time_stream().push(pfrm, app.level_timer(), e);

                // Again, the character is warping to a new location, let's
                // update its position.
                unlinked->set_grid_position(dst);

                unlinked->set_parent(parent());
                unlinked->transported();

                characters().push(std::move(unlinked));

                return;
            } else {
                ++it;
            }
        }
    }
}



void Transporter::transport_occupant(Platform& pfrm,
                                     App& app,
                                     std::optional<Vec2<u8>> destination)
{
    recharge_ = 1000 * transporter_reload_ms;

    if (parent()->interior_visible()) {
        parent()->repaint(pfrm, app);
    }

    auto chr = characters().begin();

    auto island = other_island(app);
    if (island == nullptr) {
        return;
    }

    std::optional<Vec2<u8>> dest;

    if (not destination) {
        bool matrix[16][16];
        island->plot_walkable_zones(app, matrix);

        Buffer<Vec2<u8>, 32> slots;

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

        if (&parent()->owner() == &app.player()) {
            network::packet::CharacterBoarded packet;
            packet.src_x_ = (*chr)->grid_position().x;
            packet.src_y_ = (*chr)->grid_position().y;
            packet.dst_x_ = dest->x;
            packet.dst_y_ = dest->y;
            network::transmit(pfrm, packet);
        }

        time_stream::event::CharacterTransported e;
        e.previous_x_ = (*chr)->grid_position().x;
        e.previous_y_ = (*chr)->grid_position().y;
        e.id_.set((*chr)->id());
        e.source_near_ = &parent()->owner() == &app.player();
        app.time_stream().push(pfrm, app.level_timer(), e);

        (*chr)->set_grid_position(*dest);
        (*chr)->set_parent(island);
        (*chr)->transported();

        room->characters().push(std::move(*chr));
    } else {
        return;
    }

    characters().erase(characters().begin());
}



ScenePtr<Scene> Transporter::select(Platform& pfrm, App& app)
{
    if (auto new_scene = Room::select(pfrm, app)) {
        return new_scene;
    }

    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;


    if (mt_prep_seconds) {
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
            return scene_pool::alloc<TransportCharacterScene>(position());
        } else {
            transport_occupant(pfrm, app);
        }

        return null_scene();
    } else {
        return scene_pool::alloc<RecoverCharacterScene>(position());
    }
}


void Transporter::render_interior(App& app, u8 buffer[16][16])
{
    if (recharge_) {
        buffer[position().x][position().y] = InteriorTile::transporter_recharge;
    } else {
        buffer[position().x][position().y] = InteriorTile::transporter_1;
    }
    buffer[position().x][position().y + 1] = InteriorTile::transporter_2;
}



void Transporter::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



bool Transporter::ready() const
{
    return recharge_ == 0 and
           parent()->power_supply() > parent()->power_drain();
}



} // namespace skyland
