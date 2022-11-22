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



class Mycelium final : public Room
{
public:
    Mycelium(Island* parent, const RoomCoord& position, const char* n = name());


    void render_scaffolding(App& app, TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;


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


    static Float atp_value()
    {
        return 1.f;
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


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
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


    Microseconds reload_time_remaining() const override
    {
        return flood_time - flood_timer_;
    }


protected:
    Microseconds flood_timer_ = 0;
};



} // namespace skyland
