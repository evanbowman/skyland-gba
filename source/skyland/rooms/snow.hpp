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



class Snow final : public Decoration
{
public:
    Snow(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::snow;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::snow;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::fragile |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::not_constructible;
    }


    static const char* name()
    {
        return "snow";
    }


    int debris_tile() override
    {
        return 5;
    }


    int debris_count() override
    {
        return 2;
    }


    static SystemString ui_name()
    {
        return SystemString::block_snow;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 2344;
    }


    static Icon unsel_icon()
    {
        return 2360;
    }
};



} // namespace skyland
