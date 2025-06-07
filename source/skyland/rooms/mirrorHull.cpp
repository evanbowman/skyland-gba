////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "mirrorHull.hpp"
#include "skyland/island.hpp"



namespace skyland
{



MirrorHull::MirrorHull(Island* parent, const RoomCoord& position)
    : Hull(parent, position, name())
{
}



void MirrorHull::update(Time delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void MirrorHull::rewind(Time delta)
{
    Room::rewind(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



extern SharedVariable block_crack_threshold_health;



TileId MirrorHull::tile() const
{
    if (health() <= block_crack_threshold_health) {
        return Tile::damaged_mirror_hull;
    } else {
        return Tile::mirror_hull;
    }
}



void MirrorHull::render_interior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void MirrorHull::render_exterior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



} // namespace skyland
