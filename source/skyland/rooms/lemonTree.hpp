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



class LemonTree : public Decoration
{
public:
    LemonTree(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App& app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::lemon_tree_1;
        buffer[position().x][position().y + 1] = Tile::lemon_tree_2;
    }


    void render_exterior(App& app, TileId buffer[16][16]) override
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
