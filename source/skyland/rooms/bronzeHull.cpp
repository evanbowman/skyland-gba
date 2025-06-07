////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "bronzeHull.hpp"
#include "skyland/island.hpp"



namespace skyland
{



BronzeHull::BronzeHull(Island* parent, const RoomCoord& position)
    : Hull(parent, position, name())
{
}



extern SharedVariable block_crack_threshold_health;



TileId BronzeHull::tile() const
{
    if (health() < block_crack_threshold_health) {
        return Tile::damaged_bronze_hull;
    } else {
        return Tile::bronze;
    }
}



void BronzeHull::update(Time delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void BronzeHull::rewind(Time delta)
{
    Room::rewind(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void BronzeHull::render_interior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void BronzeHull::render_exterior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



} // namespace skyland
