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



class LemonTree final : public Decoration
{
public:
    LemonTree(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::lemon_tree_1;
        buffer[position().x][position().y + 1] = Tile::lemon_tree_2;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::lemon_tree_1;
        buffer[position().x][position().y + 1] = Tile::lemon_tree_2;
    }


    static const char* name()
    {
        return "lemon-tree";
    }


    static RoomProperties::Bitmask properties()
    {
        return Decoration::properties() | RoomProperties::highly_flammable;
    }


    static SystemString ui_name()
    {
        return SystemString::block_lemon_tree;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static Icon icon()
    {
        return 1992;
    }


    static Icon unsel_icon()
    {
        return 2008;
    }
};



} // namespace skyland
