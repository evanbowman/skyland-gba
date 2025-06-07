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



class StackedHull final : public Hull
{
public:
    StackedHull(Island* parent, const RoomCoord& position);


    using Hull::Hull;


    void update(Time delta) override;
    void rewind(Time delta) override;


    TileId tile() const;


    static const char* name()
    {
        return "stacked-hull";
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static SystemString ui_name()
    {
        return SystemString::block_stacked_hull;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_stacked_hull)->c_str();
    }


    static RoomProperties::Bitmask properties()
    {
        return Hull::properties() | RoomProperties::disabled_in_tutorials |
               RoomProperties::manufactory_required |
               RoomProperties::multiboot_compatible;
    }


    static Icon icon()
    {
        return 2024;
    }


    static Icon unsel_icon()
    {
        return 2040;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;
};



} // namespace skyland
