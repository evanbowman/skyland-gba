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


#include "basalt.hpp"
#include "skyland/island.hpp"



namespace skyland
{



void Basalt::render_interior(App& app, u8 buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and above->metaclass() == metaclass()) {
        buffer[position().x][position().y] = Tile::basalt;
    } else {
        buffer[position().x][position().y] = Tile::basalt_top;
    }
}



void Basalt::render_exterior(App& app, u8 buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and above->metaclass() == metaclass()) {
        buffer[position().x][position().y] = InteriorTile::basalt;
    } else {
        buffer[position().x][position().y] = InteriorTile::basalt_top;
    }
}



} // namespace skyland
