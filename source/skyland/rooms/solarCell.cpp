////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "solarCell.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/solarStorm.hpp"



namespace skyland
{



Power SolarCell::power_usage() const
{
    const auto base_power = (*metaclass())->consumes_power();
    auto power = base_power;

    if (APP.environment().id() == weather::SolarStorm::id_) {
        power *= 2;
    } else if (APP.environment().is_overcast()) {
        power /= 2;
    } else {
        if (APP.environment().is_night()) {
            power = 0;
        }
    }

    return power;
}



void SolarCell::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_solar_cell)->c_str();
}



void SolarCell::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::solar_cell;
    buffer[position().x + 1][position().y] = InteriorTile::solar_cell;
}



void SolarCell::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::solar_cell;
    buffer[position().x + 1][position().y] = Tile::solar_cell;
}



} // namespace skyland
