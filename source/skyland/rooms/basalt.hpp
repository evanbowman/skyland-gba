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


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Basalt : public Decoration
{
public:
    Basalt(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_basalt)->c_str();
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;


    static u32 properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::not_constructible | RoomProperties::fireproof;
    }


    static const char* name()
    {
        return "basalt";
    }


    static SystemString ui_name()
    {
        return SystemString::block_basalt;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }
};



} // namespace skyland