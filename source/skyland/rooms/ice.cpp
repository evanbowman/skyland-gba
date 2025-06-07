////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "ice.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void Ice::update(Time delta)
{
    Room::update(delta);

    Room::ready();

    u8 x = position().x;
    u8 y = position().y;

    auto melt = [&] {
        if (APP.environment().is_cold()) {
            return;
        }
        __unsafe__transmute(skyland::metaclass_index("water-source"));
    };

    if (auto room = parent()->get_room({x, u8(y - 1)})) {
        if (((*room->metaclass())->properties() &
             RoomProperties::generates_heat) or
            parent()->fire_present({x, u8(y - 1)})) {
            melt();
            return;
        }
    }

    if (auto room = parent()->get_room({x, u8(y + 1)})) {
        if (((*room->metaclass())->properties() &
             RoomProperties::generates_heat) or
            parent()->fire_present({x, u8(y + 1)})) {
            melt();
            return;
        }
    }

    if (auto room = parent()->get_room({u8(x + 1), y})) {
        if (((*room->metaclass())->properties() &
             RoomProperties::generates_heat) or
            parent()->fire_present({u8(x + 1), y})) {
            melt();
            return;
        }
    }

    if (auto room = parent()->get_room({u8(x - 1), y})) {
        if (((*room->metaclass())->properties() &
             RoomProperties::generates_heat) or
            parent()->fire_present({u8(x - 1), y})) {
            melt();
            return;
        }
    }
}



void Ice::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ice;
}



void Ice::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::ice;
}



} // namespace skyland
