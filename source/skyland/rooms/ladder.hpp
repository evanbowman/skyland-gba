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



class Ladder : public Room
{
public:
    Ladder(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Time delta) override;


    static void format_description(StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override;


    static ATP atp_value()
    {
        return 50.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "ladder";
    }


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


    const char* upgrade_mt_name() const override;


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


    static void format_description(StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override;


    const char* upgrade_mt_name() const override;


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
