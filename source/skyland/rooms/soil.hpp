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


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Soil final : public Decoration
{
public:
    Soil(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_soil)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::soil;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::soil;
    }


    static RoomProperties::Bitmask properties()
    {
        return Decoration::properties() & ~RoomProperties::locked_by_default;
    }


    static const char* name()
    {
        return "soil";
    }


    static SystemString ui_name()
    {
        return SystemString::block_soil;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1416;
    }


    static Icon unsel_icon()
    {
        return 1432;
    }
};



} // namespace skyland
