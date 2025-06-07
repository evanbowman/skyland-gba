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



class Ice final : public Decoration
{
public:
    Ice(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_ice)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;


    void update(Time delta) override;


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::not_constructible | RoomProperties::fireproof;
    }


    static const char* name()
    {
        return "ice";
    }


    static SystemString ui_name()
    {
        return SystemString::block_ice;
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
