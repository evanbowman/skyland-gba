////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "radar.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Radar::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_radar)->c_str();
}



Radar::Radar(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Radar::render_interior(App* app, TileId buffer[16][16])
{

    buffer[position().x][position().y] = InteriorTile::radar_1;
    buffer[position().x][position().y + 1] = InteriorTile::radar_2;
}



void Radar::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::radar_1;
    buffer[position().x][position().y + 1] = Tile::radar_2;
}



} // namespace skyland
