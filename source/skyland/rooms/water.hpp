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


    void render_scaffolding(App& app, TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override;


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


    static Float atp_value()
    {
        return 0.1f;
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


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
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


    virtual void
    check_flood_parent(Platform& pfrm, App& app, Microseconds delta);


protected:
    Microseconds decay_ = 0;

    RoomCoord flood_parent_;
    bool has_flood_parent_ = true;

    Microseconds flood_timer_ = 0;
};



class WaterSource final : public Water
{
public:
    WaterSource(Island* parent, const RoomCoord& position);


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void
    check_flood_parent(Platform& pfrm, App& app, Microseconds delta) override;


    static SystemString ui_name()
    {
        return SystemString::block_water_source;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
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
