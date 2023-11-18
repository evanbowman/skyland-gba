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



void Reactor::update(App& app, Microseconds delta)
{
    Room::update(app, delta);
}



extern Sound core_destroyed;



void Reactor::finalize(App& app)
{
    Room::finalize(app);

    if (health() == 0) {
        core_destroyed.play(4, milliseconds(600));
        core_explosion(app, parent(), center());
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
