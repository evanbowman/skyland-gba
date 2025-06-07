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



class Sunflower final : public Decoration
{
public:
    Sunflower(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::sunflower;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::sunflower;
    }


    static RoomProperties::Bitmask properties()
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
