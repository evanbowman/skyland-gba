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



namespace skyland
{



class Hull : public Room
{
public:
    Hull(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Time delta) override;
    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::multiboot_compatible;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "hull";
    }


    int debris_tile() override
    {
        return 1;
    }


    static SystemString ui_name()
    {
        return SystemString::block_hull;
    }


    static Icon icon()
    {
        return 520;
    }


    static Icon unsel_icon()
    {
        return 504;
    }

    TileId tile() const;

protected:
    TileId last_tile_;
};



} // namespace skyland
