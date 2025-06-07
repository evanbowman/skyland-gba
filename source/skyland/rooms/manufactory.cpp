////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "manufactory.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Manufactory::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_manufactory)->c_str();
}



Manufactory::Manufactory(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Manufactory::update(Time delta)
{
    Room::update(delta);
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



void Manufactory::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



} // namespace skyland
