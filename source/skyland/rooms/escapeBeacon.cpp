////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



void EscapeBeacon::update(Time delta)
{
    Room::update(delta);

    if (is_powered_down()) {
        return;
    }

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



void EscapeBeacon::rewind(Time delta)
{
    Room::rewind(delta);

    if (is_powered_down()) {
        return;
    }

    if (timer_ > 0) {
        timer_ += delta;
        if (timer_ > escape_beacon_countdown) {
            timer_ = 0;
            activated_ = false;
            schedule_repaint();
        }
    }
}



ScenePtr<Scene> EscapeBeacon::select_impl(const RoomCoord& cursor)
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
