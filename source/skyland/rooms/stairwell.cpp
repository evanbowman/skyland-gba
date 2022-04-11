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


#include "stairwell.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"


namespace skyland
{


void Stairwell::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_stairwell)->c_str();
}


Stairwell::Stairwell(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}


void Stairwell::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}


void Stairwell::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ladder_top;
    buffer[position().x][position().y + 1] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 2] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 3] = InteriorTile::ladder_base;
}


void Stairwell::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 2] = Tile::wall_window_middle_1;
    buffer[position().x][position().y + 3] = Tile::wall_window_2;
}



void Stairwell::plot_walkable_zones(App& app, bool matrix[16][16])
{
    // All tiles in a stairwell are walkable, that's kind of the point.
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



} // namespace skyland
