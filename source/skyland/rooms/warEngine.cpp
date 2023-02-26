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


#include "warEngine.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void WarEngine::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_war_engine)->c_str();
}



WarEngine::WarEngine(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void WarEngine::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



extern Sound core_destroyed;



void WarEngine::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (health() == 0) {
        core_destroyed.play(pfrm, 4, milliseconds(600));
        core_explosion(pfrm, app, parent(), center());
    }
}



void WarEngine::plot_walkable_zones(App& app, bool matrix[16][16])
{
    auto x = position().x;
    auto y = position().y;

    matrix[x][y + 2] = true;
    matrix[x][y + 3] = true;
    matrix[x + 1][y + 3] = true;
    matrix[x + 2][y + 2] = true;
    matrix[x + 2][y + 3] = true;
}




void WarEngine::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::war_engine_1;
    buffer[position().x + 1][position().y] = InteriorTile::war_engine_2;
    buffer[position().x + 2][position().y] = InteriorTile::war_engine_3;
    buffer[position().x][position().y + 1] = InteriorTile::war_engine_4;
    buffer[position().x + 1][position().y + 1] = InteriorTile::war_engine_5;
    buffer[position().x + 2][position().y + 1] = InteriorTile::war_engine_6;
    buffer[position().x][position().y + 2] = InteriorTile::war_engine_7;
    buffer[position().x + 1][position().y + 2] = InteriorTile::war_engine_8;
    buffer[position().x + 2][position().y + 2] = InteriorTile::war_engine_9;
    buffer[position().x][position().y + 3] = InteriorTile::ladder_base;
    buffer[position().x + 1][position().y + 3] = InteriorTile::plain_floor;
    buffer[position().x + 2][position().y + 3] = InteriorTile::ladder_base;
}



void WarEngine::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_middle_2;
    buffer[x][y + 2] = Tile::wall_window_middle_1;
    buffer[x][y + 3] = Tile::wall_window_2;

    buffer[x + 1][y] = Tile::wall_window_1;
    buffer[x + 1][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 1][y + 2] = Tile::wall_window_middle_1;
    buffer[x + 1][y + 3] = Tile::wall_window_2;

    buffer[x + 2][y] = Tile::wall_window_1;
    buffer[x + 2][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 2][y + 2] = Tile::wall_window_middle_1;
    buffer[x + 2][y + 3] = Tile::wall_window_2;
}



}
