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



#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Manufactory final : public Room
{
public:
    Manufactory(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void finalize() override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::factory;
    }


    static Vec2<u8> size()
    {
        return {3, 2};
    }


    static const char* name()
    {
        return "manufactory";
    }


    static SystemString ui_name()
    {
        return SystemString::block_manufactory;
    }


    static Icon icon()
    {
        return 3736;
    }


    static Icon unsel_icon()
    {
        return 3720;
    }


    static ATP atp_value()
    {
        return 800.0_atp;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::habitable |
               RoomProperties::multiboot_compatible;
    }
};



} // namespace skyland
