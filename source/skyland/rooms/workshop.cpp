////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



void Workshop::update(Time delta)
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

        if (auto sp = ExploSpawner::create(center())) {
            sp->set_offset(-milliseconds(100));
        }
    }
}



} // namespace skyland
