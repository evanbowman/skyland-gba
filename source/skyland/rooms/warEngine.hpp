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



class WarEngine : public Room
{
public:
    WarEngine(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void finalize(Platform& pfrm, App& app) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static Float atp_value()
    {
        return 1300.f;
    }


    static Vec2<u8> size()
    {
        return {3, 4};
    }


    static const char* name()
    {
        return "war-engine";
    }


    static SystemString ui_name()
    {
        return SystemString::block_war_engine;
    }


    static Icon icon()
    {
        return 2840;
    }


    static Icon unsel_icon()
    {
        return 2824;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::has_chimney |
            RoomProperties::only_constructible_in_sandbox |
            RoomProperties::habitable | RoomProperties::destroy_quietly;
    }
};



} // namespace skyland
