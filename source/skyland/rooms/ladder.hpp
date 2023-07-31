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



class Ladder : public Room
{
public:
    Ladder(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Platform&, App&, Microseconds delta) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override;


    static Float atp_value()
    {
        return 50.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "ladder";
    }


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::multiboot_compatible;
    }


    static SystemString ui_name()
    {
        return SystemString::block_ladder;
    }


    static Icon icon()
    {
        return 3768;
    }


    static Icon unsel_icon()
    {
        return 3752;
    }
};



class LadderPlus : public Ladder
{
public:
    LadderPlus(Island* parent, const RoomCoord& position);


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override;


    static SystemString ui_name()
    {
        return SystemString::block_ladder_plus;
    }


    static Vec2<u8> size()
    {
        return {1, 3};
    }


    static const char* name()
    {
        return "ladder+";
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible;
    }
};



} // namespace skyland
