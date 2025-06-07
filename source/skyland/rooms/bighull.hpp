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

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class BigHull : public Room
{
public:
    BigHull(Island* parent, const RoomCoord& position, const char* n = name())
        : Room(parent, n, position)
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::hull;
        buffer[position().x + 1][position().y] = InteriorTile::hull;
        buffer[position().x][position().y + 1] = InteriorTile::hull;
        buffer[position().x + 1][position().y + 1] = InteriorTile::hull;
    }

    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::hull;
        buffer[position().x + 1][position().y] = Tile::hull;
        buffer[position().x][position().y + 1] = Tile::hull;
        buffer[position().x + 1][position().y + 1] = Tile::hull;
    }


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::only_constructible_in_sandbox |
               RoomProperties::not_constructible;
    }


    bool description_visible() override
    {
        return true;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_bighull)->c_str();
    }


    static ATP atp_value()
    {
        return 1.0_atp;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "big-hull";
    }


    static SystemString ui_name()
    {
        return SystemString::block_big_hull;
    }


    static Icon icon()
    {
        return 520;
    }


    static Icon unsel_icon()
    {
        return 504;
    }
};



} // namespace skyland
