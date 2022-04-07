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



#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Manufactory : public Room
{
public:
    Manufactory(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta);


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::factory;
    }


    static Vec2<u8> size()
    {
        return {3, 2};
    }


    static const char* name()
    {
        return "manufactory";
    }


    static SystemString ui_name()
    {
        return SystemString::block_manufactory;
    }


    static Icon icon()
    {
        return 776;
    }


    static Icon unsel_icon()
    {
        return 760;
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    static u32 properties()
    {
        return RoomProperties::workshop_required | RoomProperties::habitable;
    }
};



} // namespace skyland
