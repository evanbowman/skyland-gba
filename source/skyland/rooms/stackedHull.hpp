////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "hull.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class StackedHull : public Hull
{
public:
    StackedHull(Island* parent, const Vec2<u8>& position);


    using Hull::Hull;


    static const char* name()
    {
        return "stacked-hull";
    }


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
    }


    static SystemString ui_name()
    {
        return SystemString::block_stacked_hull;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_stacked_hull)->c_str();
    }


    static u32 properties()
    {
        return Hull::properties() | RoomProperties::disabled_in_tutorials |
               RoomProperties::manufactory_required;
    }


    static Icon icon()
    {
        return 2024;
    }


    static Icon unsel_icon()
    {
        return 2040;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;
};



} // namespace skyland
