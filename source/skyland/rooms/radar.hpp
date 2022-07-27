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


#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Radar : public Room
{
public:
    Radar(Island* parent, const RoomCoord& position);



    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);



    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;


    void render_scaffolding(App& app, TileId buffer[16][16]) override
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


    static Float atp_value()
    {
        return 400.f;
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
               RoomProperties::disallow_chimney | RoomProperties::roof_hidden;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }
};



} // namespace skyland
