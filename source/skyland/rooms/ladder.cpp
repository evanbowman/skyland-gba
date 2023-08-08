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


#include "ladder.hpp"
#include "platform/platform.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/upgradePromptScene.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



const char* Ladder::upgrade_mt_name() const
{
    return "ladder+";
}



void Ladder::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_ladder)->c_str();
}



void LadderPlus::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_ladder_plus)->c_str();
}



Ladder::Ladder(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Ladder::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
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



void LadderPlus::plot_walkable_zones(App& app,
                                     bool matrix[16][16],
                                     BasicCharacter* for_character)
{
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



void Ladder::plot_walkable_zones(App& app,
                                 bool matrix[16][16],
                                 BasicCharacter* for_character)
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



const char* LadderPlus::upgrade_mt_name() const
{
    return "stairwell";
}



} // namespace skyland
