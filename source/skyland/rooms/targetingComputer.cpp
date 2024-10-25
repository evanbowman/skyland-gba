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



void TargetingComputer::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_targeting_computer)->c_str();
}



void TargetingComputer::update(Time delta)
{
    Room::update(delta);

    if (not enabled_) {
        // As long as the targeting computer is turned off, delay autofire, if
        // enabled, indefinitely, until the computer is destroyed or turned back
        // on.
        Room::ready();
        if (APP.gp_.stateflags_.get(GlobalPersistentData::autofire_on)) {
            APP.player().delay_autofire(seconds(1));
        }
        return;
    }

    if (is_powered_down()) {
        return;
    }

    Room::ready();

    if (&parent()->owner() == &APP.opponent()) {
        if (APP.game_mode() == App::GameMode::multiplayer) {
            return;
        }
        apply_damage(Room::health_upper_limit());
        return;
    }

    APP.player().update_weapon_targets(delta);
}



void TargetingComputer::display_on_hover(Platform::Screen& screen,
                                         const RoomCoord& cursor)
{
    for (auto& room : parent()->rooms()) {
        if ((*room->metaclass())->category() == Room::Category::weapon) {
            room->display_on_hover(screen, room->position());
        }
    }
}



void TargetingComputer::unset_target()
{
}



ScenePtr TargetingComputer::select_impl(const RoomCoord& cursor)
{
    if (parent() == &player_island()) {
        // I repurposed the ReloadComplete event because it's already looped
        // into a callback (overriden below).
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

    enabled_ = not enabled_;

    if (enabled_) {
        PLATFORM.speaker().play_sound("poweron.raw", 4);
    } else {
        PLATFORM.speaker().play_sound("powerdown.raw", 4);
    }

    schedule_repaint();

    Room::ready();

    return null_scene();
}



void TargetingComputer::___rewind___finished_reload()
{
    enabled_ = not enabled_;
    schedule_repaint();
}



void TargetingComputer::render_interior(App* app, TileId buffer[16][16])
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



void TargetingComputer::render_exterior(App* app, TileId buffer[16][16])
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
