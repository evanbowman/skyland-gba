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



class WarEngine : public Room
{
public:
    WarEngine(Island* parent,
              const RoomCoord& position,
              const char* n = name());


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void finalize() override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static ATP atp_value()
    {
        return 1210.0_atp;
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
