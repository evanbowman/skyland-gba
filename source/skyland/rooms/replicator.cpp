////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "replicator.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/replicatorSelectionScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Replicator::Replicator(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Replicator::update(Time delta)
{
    Room::update(delta);
}



void Replicator::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_replicator)->c_str();
}



bool Replicator::create_replicant()
{
    int character_count = 0;

    Character* found_chr = nullptr;

    for (auto& chr : characters()) {
        if (chr->owner() == &parent()->owner()) {
            character_count++;
            found_chr = chr.get();
        }
    }

    if (character_count == 1) {
        const auto chr_pos = found_chr->grid_position();
        const auto chr_health = found_chr->health();

        const auto reduced_hp = int(chr_health * 0.75f);

        Health replicant_health = clamp(reduced_hp, 255 / 10 - 10, 255);

        const auto dst = [&] {
            if (chr_pos.x ==
                position().x) { // We have two slots where we can place
                                // the replicant.
                return RoomCoord{u8(chr_pos.x + 1), chr_pos.y};
            } else {
                return RoomCoord{u8(chr_pos.x - 1), chr_pos.y};
            }
        }();

        auto chr = APP.alloc_entity<Character>(
            parent(), found_chr->owner(), dst, true);

        if (chr) {
            network::packet::ReplicantCreated packet;
            packet.src_x_ = dst.x;
            packet.src_y_ = dst.y;
            packet.health_ = replicant_health;
            packet.chr_id_.set(chr->id());
            network::transmit(packet);


            time_stream::event::ReplicantCreated e;
            e.x_ = dst.x;
            e.y_ = dst.y;
            e.near_ = is_player_island(parent());
            e.owned_by_player_ = found_chr->owner() == &APP.player();
            APP.time_stream().push(APP.level_timer(), e);

            chr->apply_damage(255 - replicant_health);

            if (found_chr->get_race() == Character::Race::dog) {
                chr->set_race(Character::Race::dog);
            }

            chr->set_max_health(replicant_health);
            chr->transported();
            edit_characters().push(std::move(chr));
            update_description();

            PLATFORM.speaker().play_sound("transporter", 4);
        }

    } else {
        return false;
    }

    return true;
}



ScenePtr Replicator::select_impl(const RoomCoord& cursor)
{
    if (auto next = Room::select_impl(cursor)) {
        return next;
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return make_scene<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_power_out);
        return make_scene<NotificationScene>(str->c_str(), future_scene);
    }

    if (parent() == APP.opponent_island() and
        APP.game_mode() not_eq App::GameMode::sandbox) {
        return null_scene();
    }

    Character* found_chr = nullptr;

    for (auto& chr : characters()) {
        if (chr->owner() == &parent()->owner()) {
            found_chr = chr.get();
        }
    }

    if (found_chr) {

        using Next = ReplicatorSelectionScene;

        const bool near = is_player_island(parent());
        auto next = make_deferred_scene<Next>(near);

        if (APP.game_mode() == App::GameMode::co_op) {
            return co_op_acquire_lock(next);
        } else {
            return next();
        }
    }

    return null_scene();
}



void Replicator::render_interior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = InteriorTile::replicator_1;
    buffer[x][y + 1] = InteriorTile::replicator_3;

    buffer[x + 1][y] = InteriorTile::replicator_2;
    buffer[x + 1][y + 1] = InteriorTile::replicator_4;

    buffer[x][y + 2] = InteriorTile::plain_floor;
    buffer[x + 1][y + 2] = InteriorTile::plain_floor;
}



void Replicator::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_middle_2;
    buffer[x][y + 2] = Tile::wall_plain_2;

    buffer[x + 1][y] = Tile::wall_window_1;
    buffer[x + 1][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 1][y + 2] = Tile::wall_plain_2;
}



} // namespace skyland
