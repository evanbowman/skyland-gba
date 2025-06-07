////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "poweredHull.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void PoweredHull::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_energized_hull)->c_str();
}



PoweredHull::PoweredHull(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void PoweredHull::update(Time delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void PoweredHull::rewind(Time delta)
{
    Room::rewind(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



extern SharedVariable block_crack_threshold_health;



TileId PoweredHull::tile() const
{
    if (health() <= block_crack_threshold_health) {
        return Tile::damaged_energized_hull;
    } else {
        return Tile::field_hull;
    }
}



void PoweredHull::render_interior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void PoweredHull::render_exterior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



} // namespace skyland
