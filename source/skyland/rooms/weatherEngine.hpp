#pragma once


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



class WeatherEngine final : public Room
{
public:
    WeatherEngine(Island* parent,
                  const RoomCoord& position,
                  const char* n = name());


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::disallow_chimney |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::locked_by_default;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "weather-engine";
    }


    static SystemString ui_name()
    {
        return SystemString::block_weather_engine;
    }


    static Icon icon()
    {
        return 3416;
    }


    static Icon unsel_icon()
    {
        return 3432;
    }
};



} // namespace skyland
