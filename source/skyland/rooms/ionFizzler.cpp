////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "ionFizzler.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void IonFizzler::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_ion_fizzler)->c_str();
}



IonFizzler::IonFizzler(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void IonFizzler::update(Time delta)
{
    Room::update(delta);
}



void IonFizzler::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ion_fizzler;
    buffer[position().x][position().y + 1] = InteriorTile::ion_fizzler_interior;
}



void IonFizzler::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::ion_fizzler;
    buffer[position().x][position().y + 1] = Tile::ion_fizzler_exterior;
}



} // namespace skyland
