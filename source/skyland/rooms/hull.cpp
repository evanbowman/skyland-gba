////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "hull.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Hull::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_hull)->c_str();
}



Hull::Hull(Island* parent, const Vec2<u8>& position, const char* n)
    : Room(parent, n, position)
{
}



void Hull::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Hull::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::hull;
}



void Hull::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::hull;
}



} // namespace skyland
