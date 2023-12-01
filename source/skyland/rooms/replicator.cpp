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


#include "replicator.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
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



void Replicator::update(Microseconds delta)
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

    BasicCharacter* found_chr = nullptr;

    for (auto& chr : characters()) {
        if (chr->owner() == &parent()->owner()) {
            character_count++;
            found_chr = chr.get();
        }
    }

    if (character_count == 1) {
        const auto chr_pos = found_chr->grid_position();
        const auto chr_health = found_chr->health();

        const Health replicant_health = chr_health * 0.75f;

        const auto dst = [&] {
            if (chr_pos.x ==
                position().x) { // We have two slots where we can place
                                // the replicant.
                return RoomCoord{u8(chr_pos.x + 1), chr_pos.y};
            } else {
                return RoomCoord{u8(chr_pos.x - 1), chr_pos.y};
            }
        }();

        auto chr = APP.alloc_entity<BasicCharacter>(
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
            chr->transported();
            edit_characters().push(std::move(chr));
            update_description();
        }

    } else {
        return false;
    }

    return true;
}



ScenePtr<Scene> Replicator::select(const RoomCoord& cursor)
{
    if (auto next = Room::select(cursor)) {
        return next;
    }

    if (parent() == APP.opponent_island() and
        APP.game_mode() not_eq App::GameMode::sandbox) {
        return null_scene();
    }

    int character_count = 0;

    BasicCharacter* found_chr = nullptr;

    for (auto& chr : characters()) {
        if (chr->owner() == &parent()->owner()) {
            character_count++;
            found_chr = chr.get();
        }
    }

    if (found_chr) {

        using Next = ReplicatorSelectionScene;

        const bool near = is_player_island(parent());
        auto next = scene_pool::make_deferred_scene<Next>(near);

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

    buffer[x][y] = InteriorTile::empty;
    buffer[x + 1][y] = InteriorTile::empty;

    buffer[x][y + 1] = InteriorTile::replicator_1;
    buffer[x][y + 2] = InteriorTile::replicator_3;

    buffer[x + 1][y + 1] = InteriorTile::replicator_2;
    buffer[x + 1][y + 2] = InteriorTile::replicator_4;

    buffer[x][y + 3] = InteriorTile::plain_floor;
    buffer[x + 1][y + 3] = InteriorTile::plain_floor;
}



void Replicator::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_middle_2;
    buffer[x][y + 2] = Tile::wall_window_middle_1;
    buffer[x][y + 3] = Tile::wall_window_2;

    buffer[x + 1][y] = Tile::wall_window_1;
    buffer[x + 1][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 1][y + 2] = Tile::wall_window_middle_1;
    buffer[x + 1][y + 3] = Tile::wall_window_2;
}



} // namespace skyland
