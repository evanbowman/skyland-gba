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

#include "hull.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class BronzeHull final : public Hull
{
public:
    BronzeHull(Island* parent, const RoomCoord& position);


    using Hull::Hull;


    int debris_tile() override
    {
        return 2;
    }


    static const char* name()
    {
        return "bronze-hull";
    }


    static SystemString ui_name()
    {
        return SystemString::block_bronze_hull;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_bronze_hull)->c_str();
    }


    static RoomProperties::Bitmask properties()
    {
        return Hull::properties() | RoomProperties::disabled_in_tutorials |
               RoomProperties::locked_by_default |
               RoomProperties::multiboot_compatible;
    }


    static Icon icon()
    {
        return 1624;
    }


    static Icon unsel_icon()
    {
        return 1640;
    }


    TileId tile() const;


    void update(Time delta) override;
    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;
};



} // namespace skyland
