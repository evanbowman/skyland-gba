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
#include "skyland/tile.hpp"



namespace skyland
{



class Barrier : public Room
{
public:
    Barrier(Island* parent, const RoomCoord& position);


    void update(Platform&, App&, Microseconds delta) override;


    void apply_damage(Platform&, App&, Health damage) override;


    static const char* name()
    {
        return "barrier";
    }


    static SystemString ui_name()
    {
        return SystemString::block_barrier;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_barrier)->c_str();
    }


    static Float ai_base_weight()
    {
        return 0.00001f;
    }


    bool description_visible() override
    {
        return true;
    }


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::sandbox_mode_only | RoomProperties::fireproof;
    }


    static Icon icon()
    {
        return 2088;
    }


    static Icon unsel_icon()
    {
        return 2104;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    void render_scaffolding(App& app, TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void render_interior(App& app, TileId buffer[16][16]) override;


    void render_exterior(App& app, TileId buffer[16][16]) override;
};



} // namespace skyland
