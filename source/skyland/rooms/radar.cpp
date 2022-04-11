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


#include "radar.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Radar::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_radar)->c_str();
}



Radar::Radar(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Radar::render_interior(App& app, TileId buffer[16][16])
{

    buffer[position().x][position().y] = InteriorTile::radar_1;
    buffer[position().x][position().y + 1] = InteriorTile::radar_2;
}



void Radar::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::radar_1;
    buffer[position().x][position().y + 1] = Tile::radar_2;
}



} // namespace skyland
