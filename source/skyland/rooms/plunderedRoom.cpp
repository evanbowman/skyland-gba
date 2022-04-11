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


#include "plunderedRoom.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void PlunderedRoom::format_description(Platform& pfrm,
                                       StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_plundered_room)->c_str();
}



PlunderedRoom::PlunderedRoom(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}


void PlunderedRoom::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void PlunderedRoom::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::plundered_room;
    buffer[position().x][position().y + 1] = InteriorTile::plundered_room;
}



void PlunderedRoom::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::plundered_room;
    buffer[position().x][position().y + 1] = Tile::plundered_room;
}



} // namespace skyland
