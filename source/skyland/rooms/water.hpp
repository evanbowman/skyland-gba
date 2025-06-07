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

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Water : public Room
{
public:
    Water(Island* parent, const RoomCoord& position, const char* n = name());


    void set_flood_parent(RoomCoord parent)
    {
        flood_parent_ = parent;
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible | RoomProperties::roof_hidden |
               RoomProperties::fluid | RoomProperties::fragile |
               RoomProperties::destroy_quietly | RoomProperties::fireproof;
    }


    static ATP atp_value()
    {
        return 0.1_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "water";
    }


    static SystemString ui_name()
    {
        return SystemString::block_water;
    }


    bool description_visible() override
    {
        return true;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_water)->c_str();
    }


    static Icon icon()
    {
        return 2120;
    }


    static Icon unsel_icon()
    {
        return 2136;
    }


    void refresh()
    {
        decay_ = 0;
    }


    virtual void check_flood_parent(Time delta);


protected:
    Time decay_ = 0;

    RoomCoord flood_parent_;
    bool has_flood_parent_ = true;

    Time flood_timer_ = 0;
};



class WaterSource final : public Water
{
public:
    WaterSource(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void check_flood_parent(Time delta) override;


    static SystemString ui_name()
    {
        return SystemString::block_water_source;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_water_source)->c_str();
    }


    static const char* name()
    {
        return "water-source";
    }


    static RoomProperties::Bitmask properties()
    {
        return Water::properties() & ~RoomProperties::not_constructible;
    }
};



} // namespace skyland
