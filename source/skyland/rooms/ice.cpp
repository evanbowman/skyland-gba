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


#include "ice.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void Ice::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    u8 x = position().x;
    u8 y = position().y;

    auto melt = [&] {
        __unsafe__transmute(
            pfrm, app, skyland::metaclass_index("water-source"));
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



void Ice::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ice;
}



void Ice::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::ice;
}



} // namespace skyland
