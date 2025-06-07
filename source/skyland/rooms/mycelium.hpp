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

#include "skyland/bulkTimer.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Mycelium final : public Room, public Timer
{
public:
    Mycelium(Island* parent, const RoomCoord& position, const char* n = name());


    ~Mycelium();


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void timer_expired() override;


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disabled_in_tutorials |
               RoomProperties::flag_mount | RoomProperties::workshop_required |
               RoomProperties::locked_by_default | RoomProperties::roof_hidden |
               RoomProperties::salvage_disallowed |
               RoomProperties::highly_flammable;
    }


    static ATP atp_value()
    {
        return 1.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "mycelium";
    }


    static SystemString ui_name()
    {
        return SystemString::block_mycelium;
    }


    bool description_visible() override
    {
        return true;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_mycelium)->c_str();
    }


    static Icon icon()
    {
        return 2184;
    }


    static Icon unsel_icon()
    {
        return 2200;
    }


    static constexpr const auto flood_time = seconds(8);


    Time reload_time_remaining() const override
    {
        return Timer::remaining();
    }
};



} // namespace skyland
