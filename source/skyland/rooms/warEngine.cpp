////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



void WarEngine::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_war_engine)->c_str();
}



WarEngine::WarEngine(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void WarEngine::update(Time delta)
{
    Room::update(delta);
}



extern Sound core_destroyed;



void WarEngine::finalize()
{
    Room::finalize();

    if (health() == 0) {
        core_destroyed.play(4, milliseconds(600));
        core_explosion(parent(), center(), CoreExplosionConfig{.arms_ = 6});
    }
}



void WarEngine::plot_walkable_zones(bool matrix[16][16],
                                    Character* for_character)
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



} // namespace skyland
