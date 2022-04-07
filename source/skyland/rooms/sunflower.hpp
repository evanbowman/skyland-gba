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


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Sunflower : public Decoration
{
public:
    Sunflower(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::sunflower;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::sunflower;
    }


    static u32 properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::highly_flammable;
    }


    static const char* name()
    {
        return "sunflower";
    }


    static SystemString ui_name()
    {
        return SystemString::block_sunflower;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1896;
    }


    static Icon unsel_icon()
    {
        return 1912;
    }
};



} // namespace skyland
