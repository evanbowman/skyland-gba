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


#include "solarCell.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland
{



Power SolarCell::power_usage() const
{
    const auto base_power = (*metaclass())->consumes_power();
    auto power = base_power;

    if (parent()->get_room({position().x, u8(position().y - 1)})) {
        power -= base_power / 4;
    }

    if (parent()->get_room({u8(position().x + 1), u8(position().y - 1)})) {
        power -= base_power / 4;
    }

    return power;
}



void SolarCell::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_solar_cell)->c_str();
}



void SolarCell::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::solar_cell;
    buffer[position().x + 1][position().y] = InteriorTile::solar_cell;
}



void SolarCell::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::solar_cell;
    buffer[position().x + 1][position().y] = Tile::solar_cell;
}



} // namespace skyland
