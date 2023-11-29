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


#include "escapeBeacon.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



EscapeBeacon::EscapeBeacon(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void EscapeBeacon::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_escape_beacon)->c_str();
}



void EscapeBeacon::update(Microseconds delta)
{
    Room::update(delta);

    if (activated_) {

        Room::ready();

        if (timer_ >= 0) {
            timer_ -= delta;

            if (timer_ <= 0) {

                if (parent() == &player_island()) {
                    APP.exit_condition() = App::ExitCondition::player_fled;
                } else {
                    APP.exit_condition() = App::ExitCondition::opponent_fled;
                }

                activated_ = false;
                timer_ = 0;
            }
        }
    }
}



static const auto escape_beacon_countdown = seconds(60);



void EscapeBeacon::rewind(Microseconds delta)
{
    Room::rewind(delta);

    if (timer_ > 0) {
        timer_ += delta;
        if (timer_ > escape_beacon_countdown) {
            timer_ = 0;
            activated_ = false;
            schedule_repaint();
        }
    }
}



ScenePtr<Scene> EscapeBeacon::select(const RoomCoord& cursor)
{
    if (APP.game_mode() == App::GameMode::adventure) {
        if (APP.world_graph().nodes_[APP.current_world_location()].type_ ==
            WorldGraph::Node::Type::corrupted) {
            PLATFORM.speaker().play_sound("beep_error", 3);
            // You can't run from the boss.
            return null_scene();
        }
    }

    const bool was_activated = activated_;

    activated_ = true;

    if (not was_activated) {
        schedule_repaint();
        timer_ = escape_beacon_countdown;
    }

    Room::ready();

    return null_scene();
}



void EscapeBeacon::render_interior(App* app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    if (activated_) {
        buffer[x][y] = InteriorTile::escape_beacon_on_1;
        buffer[x][y + 1] = InteriorTile::escape_beacon_on_2;
    } else {
        buffer[x][y] = InteriorTile::escape_beacon_off_1;
        buffer[x][y + 1] = InteriorTile::escape_beacon_off_2;
    }


    buffer[x][y + 2] = InteriorTile::escape_beacon_base;
}



void EscapeBeacon::render_exterior(App* app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    if (activated_) {
        buffer[x][y] = Tile::escape_beacon_on_1;
        buffer[x][y + 1] = Tile::escape_beacon_on_2;
    } else {
        buffer[x][y] = Tile::escape_beacon_off_1;
        buffer[x][y + 1] = Tile::escape_beacon_off_2;
    }


    buffer[x][y + 2] = InteriorTile::escape_beacon_base;
}



} // namespace skyland
