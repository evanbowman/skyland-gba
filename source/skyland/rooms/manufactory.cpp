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


#include "manufactory.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Manufactory::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_manufactory)->c_str();
}



Manufactory::Manufactory(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Manufactory::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Manufactory::render_interior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = InteriorTile::manufactory_1;
    buffer[x + 1][y] = InteriorTile::manufactory_2;
    buffer[x + 2][y] = InteriorTile::manufactory_3;

    buffer[x][y + 1] = InteriorTile::manufactory_4;
    buffer[x + 1][y + 1] = InteriorTile::manufactory_5;
    buffer[x + 2][y + 1] = InteriorTile::plain_floor;
}



void Manufactory::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_2;
    buffer[x + 1][y] = Tile::wall_plain_1;
    buffer[x + 1][y + 1] = Tile::wall_plain_2;
    buffer[x + 2][y] = Tile::wall_window_1;
    buffer[x + 2][y + 1] = Tile::wall_window_2;
}



void Manufactory::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (health() <= 0) {
        ExploSpawner::create(pfrm, app, center());
    }
}



} // namespace skyland
