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


#include "workshop.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



const char* Workshop::upgrade_mt_name() const
{
    return "manufactory";
}



void Workshop::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_workshop)->c_str();
}



Workshop::Workshop(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Workshop::update(Microseconds delta)
{
    Room::update(delta);
}



void Workshop::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::workshop_1;
    buffer[position().x][position().y + 1] = InteriorTile::workshop_2;
    buffer[position().x + 1][position().y] = InteriorTile::workshop_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::workshop_4;
}



void Workshop::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



void Workshop::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



} // namespace skyland
