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


#include "barrier.hpp"



namespace skyland
{



Barrier::Barrier(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Barrier::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Barrier::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::barrier;
}



void Barrier::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::barrier;
}



void Barrier::apply_damage(Platform& pfrm, App& app, Health damage)
{
    // Takes no damage, kind of the whole point.
    Room::apply_damage(pfrm, app, 0);
}



} // namespace skyland
