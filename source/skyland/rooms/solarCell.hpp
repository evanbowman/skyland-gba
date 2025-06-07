////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class SolarCell final : public Room
{
public:
    SolarCell(Island* parent, const RoomCoord& position)
        : Room(parent, name(), position)
    {
    }


    Power power_usage() const override;


    bool description_visible() override
    {
        return true;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static ATP atp_value()
    {
        return 1000.0_atp;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static const char* name()
    {
        return "solar-cell";
    }


    static SystemString ui_name()
    {
        return SystemString::block_solar_cell;
    }


    static Icon icon()
    {
        return 2280;
    }


    static Icon unsel_icon()
    {
        return 2296;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::roof_hidden |
               RoomProperties::disallow_chimney | RoomProperties::human_only;
    }
};



} // namespace skyland
