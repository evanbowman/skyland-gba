////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "ladder.hpp"
#include "platform/platform.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/upgradePromptScene.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Optional<Room::UpgradeList> Ladder::upgrade_mt_list() const
{
    UpgradeList upgrades;
    upgrades.push_back(skyland::metaclass_index("ladder+"));
    upgrades.push_back(skyland::metaclass_index("stairwell"));
    upgrades.push_back(skyland::metaclass_index("stairwell+"));
    upgrades.push_back(skyland::metaclass_index("stairwell++"));
    return upgrades;
}



void Ladder::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_ladder)->c_str();
}



void LadderPlus::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_ladder_plus)->c_str();
}



Ladder::Ladder(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Ladder::update(Time delta)
{
    Room::update(delta);
}



void Ladder::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ladder_top;
    buffer[position().x][position().y + 1] = InteriorTile::ladder_base;
}



void Ladder::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



void LadderPlus::plot_walkable_zones(bool matrix[16][16],
                                     Character* for_character)
{
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



void Ladder::plot_walkable_zones(bool matrix[16][16], Character* for_character)
{
    // All tiles in a ladder are walkable, that's kind of the point.
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



void LadderPlus::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ladder_top;
    buffer[position().x][position().y + 1] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 2] = InteriorTile::ladder_base;
}



void LadderPlus::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 2] = Tile::wall_plain_2;
}



LadderPlus::LadderPlus(Island* parent, const RoomCoord& position)
    : Ladder(parent, position, name())
{
}



Optional<Room::UpgradeList> LadderPlus::upgrade_mt_list() const
{
    UpgradeList upgrades;
    upgrades.push_back(skyland::metaclass_index("stairwell"));
    upgrades.push_back(skyland::metaclass_index("stairwell+"));
    upgrades.push_back(skyland::metaclass_index("stairwell++"));
    return upgrades;
}



} // namespace skyland
