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


#include "targetingComputer.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/player/opponent/enemyAI.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStream.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



TargetingComputer::TargetingComputer(Island* parent,
                                     const RoomCoord& position,
                                     const char* n)
    : Room(parent, n, position)
{
}



void TargetingComputer::format_description(Platform& pfrm,
                                           StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_targeting_computer)->c_str();
}



void TargetingComputer::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (not enabled_) {
        return;
    }

    Room::ready();

    if (&parent()->owner() == &app.opponent()) {
        apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }

    if (app.opponent().is_friendly()) {
        return;
    }

    // if (app.gp_.difficulty_ not_eq GlobalPersistentData::Difficulty::beginner) {
    //     apply_damage(pfrm, app, Room::health_upper_limit());
    //     return;
    // }

    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return;
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        return;
    }

    if (next_action_timer_ > 0) {
        next_action_timer_ -= delta;
    } else {
        if (room_update_index_ >= player_island(app).rooms().size()) {
            room_update_index_ = 0;
            next_action_timer_ = seconds(3);
        } else {
            auto& room = *player_island(app).rooms()[room_update_index_++];
            if (&room not_eq this and room.metaclass() == this->metaclass()) {
                // Player built two targeting computers.
                room.apply_damage(pfrm, app, Room::health_upper_limit());
            }
            const auto category = (*room.metaclass())->category();
            if (category == Room::Category::weapon) {
                EnemyAI::update_room(pfrm,
                                     app,
                                     room,
                                     app.opponent_island()->rooms_plot(),
                                     &app.player(),
                                     &app.player_island(),
                                     app.opponent_island());
            }
            next_action_timer_ = milliseconds(32);
        }
    }
}



ScenePtr<Scene>
TargetingComputer::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    if (parent() == &player_island(app)) {
        // I repurposed the ReloadComplete event because it's already looped
        // into a callback (overriden below).
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

    enabled_ = not enabled_;

    parent()->schedule_repaint();

    Room::ready();

    return null_scene();
}



void TargetingComputer::___rewind___finished_reload(Platform& pfrm, App& app)
{
    enabled_ = not enabled_;
    parent()->schedule_repaint();
}



void TargetingComputer::render_interior(App& app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    if (enabled_) {
        buffer[x][y] = InteriorTile::targeting_computer_1;
        buffer[x][y + 1] = InteriorTile::targeting_computer_2;
    } else {
        buffer[x][y] = InteriorTile::targeting_computer_3;
        buffer[x][y + 1] = InteriorTile::targeting_computer_4;
    }
}



void TargetingComputer::render_exterior(App& app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    if (enabled_) {
        buffer[x][y] = Tile::targeting_computer_1;
        buffer[x][y + 1] = Tile::targeting_computer_2;
    } else {
        buffer[x][y] = Tile::targeting_computer_3;
        buffer[x][y + 1] = Tile::targeting_computer_4;
    }
}



} // namespace skyland
