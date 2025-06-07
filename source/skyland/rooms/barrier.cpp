////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "barrier.hpp"



namespace skyland
{



Barrier::Barrier(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Barrier::update(Time delta)
{
    Room::update(delta);
}



void Barrier::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::barrier;
}



void Barrier::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::barrier;
}



void Barrier::apply_damage(Health damage, const DamageConfiguration& conf)
{
    // Takes no damage, kind of the whole point.
    Room::apply_damage(0, conf);
}



} // namespace skyland
