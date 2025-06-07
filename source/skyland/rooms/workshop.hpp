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
#include "skyland/systemString.hpp"



namespace skyland
{



class Workshop final : public Room
{
public:
    Workshop(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void finalize() override;


    static void format_description(StringBuffer<512>& buffer);


    const char* upgrade_mt_name() const override;


    static Category category()
    {
        return Category::factory;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "workshop";
    }


    static SystemString ui_name()
    {
        return SystemString::block_workshop;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable | RoomProperties::multiboot_compatible;
    }


    int debris_tile() override
    {
        return 2;
    }


    static Icon icon()
    {
        return 776;
    }


    static Icon unsel_icon()
    {
        return 760;
    }


    static ATP atp_value()
    {
        return 700.0_atp;
    }
};



} // namespace skyland
