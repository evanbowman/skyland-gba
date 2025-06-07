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



class LadyLiberty final : public Decoration
{
public:
    LadyLiberty(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_lady_liberty)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::liberty_1;
        buffer[position().x + 1][position().y] = InteriorTile::liberty_2;
        buffer[position().x][position().y + 1] = InteriorTile::liberty_3;
        buffer[position().x + 1][position().y + 1] = InteriorTile::liberty_4;
        buffer[position().x][position().y + 2] = InteriorTile::liberty_5;
        buffer[position().x + 1][position().y + 2] = InteriorTile::liberty_6;
        buffer[position().x][position().y + 3] = InteriorTile::liberty_7;
        buffer[position().x + 1][position().y + 3] = InteriorTile::liberty_8;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::liberty_1;
        buffer[position().x + 1][position().y] = Tile::liberty_2;
        buffer[position().x][position().y + 1] = Tile::liberty_3;
        buffer[position().x + 1][position().y + 1] = Tile::liberty_4;
        buffer[position().x][position().y + 2] = Tile::liberty_5;
        buffer[position().x + 1][position().y + 2] = Tile::liberty_6;
        buffer[position().x][position().y + 3] = Tile::liberty_7;
        buffer[position().x + 1][position().y + 3] = Tile::liberty_8;
    }


    static const char* name()
    {
        return "lady-liberty";
    }


    static SystemString ui_name()
    {
        return SystemString::block_lady_liberty;
    }


    static Vec2<u8> size()
    {
        return {2, 4};
    }


    static Icon icon()
    {
        return 2056;
    }


    static Icon unsel_icon()
    {
        return 2072;
    }
};



} // namespace skyland
