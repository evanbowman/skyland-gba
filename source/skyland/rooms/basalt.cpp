////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "basalt.hpp"
#include "skyland/island.hpp"



namespace skyland
{



void Basalt::render_interior(App* app, TileId buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and above->metaclass() == metaclass()) {
        buffer[position().x][position().y] = Tile::basalt;
    } else {
        buffer[position().x][position().y] = Tile::basalt_top;
    }
}



void Basalt::render_exterior(App* app, TileId buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and above->metaclass() == metaclass()) {
        buffer[position().x][position().y] = InteriorTile::basalt;
    } else {
        buffer[position().x][position().y] = InteriorTile::basalt_top;
    }
}



} // namespace skyland
