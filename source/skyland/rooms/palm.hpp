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



class Palm final : public Decoration
{
public:
    Palm(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::palm_1;
        buffer[position().x][position().y + 1] = Tile::palm_2;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::palm_1;
        buffer[position().x][position().y + 1] = Tile::palm_2;
    }


    static RoomProperties::Bitmask properties()
    {
        return Decoration::properties() | RoomProperties::highly_flammable;
    }


    static const char* name()
    {
        return "coconut-palm";
    }


    static SystemString ui_name()
    {
        return SystemString::block_palm;
    }


    int debris_tile() override
    {
        return 4;
    }


    int debris_count() override
    {
        return 2;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static Icon icon()
    {
        return 1384;
    }


    static Icon unsel_icon()
    {
        return 1400;
    }
};



} // namespace skyland
