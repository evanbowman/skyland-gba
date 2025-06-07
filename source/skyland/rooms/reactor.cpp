////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "reactor.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Reactor::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_reactor)->c_str();
}



Reactor::Reactor(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Reactor::update(Time delta)
{
    Room::update(delta);
}



extern Sound core_destroyed;



void Reactor::finalize()
{
    Room::finalize();

    if (health() == 0) {
        core_destroyed.play(4, milliseconds(600));
        core_explosion(parent(),
                       center(),
                       CoreExplosionConfig{
                           .arms_ = 5,
                           .rot_ = rng::choice<45>(rng::utility_state),
                       });
    }
}



void Reactor::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::reactor_1;
    buffer[position().x + 1][position().y] = InteriorTile::reactor_2;

    buffer[position().x][position().y + 1] = InteriorTile::reactor_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::reactor_4;

    buffer[position().x][position().y + 2] = InteriorTile::core_2;
    buffer[position().x + 1][position().y + 2] = InteriorTile::plain_floor;
}



void Reactor::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_middle_2;
    buffer[x][y + 2] = Tile::wall_plain_2;

    buffer[x + 1][y] = Tile::wall_window_1;
    buffer[x + 1][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 1][y + 2] = Tile::wall_plain_2;
}



} // namespace skyland
