////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "plunderedRoom.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void PlunderedRoom::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_plundered_room)->c_str();
}



PlunderedRoom::PlunderedRoom(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}


void PlunderedRoom::update(Time delta)
{
    Room::update(delta);
}



void PlunderedRoom::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::plundered_room;
    buffer[position().x][position().y + 1] = InteriorTile::plundered_room;
}



void PlunderedRoom::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::plundered_room;
    buffer[position().x][position().y + 1] = Tile::plundered_room;
}



} // namespace skyland
