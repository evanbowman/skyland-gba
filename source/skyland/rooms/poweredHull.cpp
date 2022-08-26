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


#include "poweredHull.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void PoweredHull::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_energized_hull)->c_str();
}



PoweredHull::PoweredHull(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void PoweredHull::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void PoweredHull::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

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



void PoweredHull::render_interior(App& app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void PoweredHull::render_exterior(App& app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



} // namespace skyland
