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



namespace skyland
{



class Radar final : public Room
{
public:
    Radar(Island* parent, const RoomCoord& position);



    static void format_description(StringBuffer<512>& buffer);



    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }



    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "radar";
    }


    static SystemString ui_name()
    {
        return SystemString::block_radar;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 400.0_atp;
    }


    static Icon icon()
    {
        return 872;
    }


    static Icon unsel_icon()
    {
        return 856;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required |
               RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::multiboot_compatible;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }
};



} // namespace skyland
