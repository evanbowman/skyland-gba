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



class Reactor final : public Room
{
public:
    Reactor(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void finalize() override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static ATP atp_value()
    {
        return 1200.0_atp;
    }


    static Vec2<u8> size()
    {
        return {2, 3};
    }


    static const char* name()
    {
        return "reactor";
    }


    static SystemString ui_name()
    {
        return SystemString::block_reactor;
    }


    static Icon icon()
    {
        return 3704;
    }


    static Icon unsel_icon()
    {
        return 3688;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::has_chimney | RoomProperties::destroy_quietly |
               RoomProperties::accepts_ion_damage | RoomProperties::habitable |
               RoomProperties::multiboot_compatible |
               RoomProperties::human_only;
    }
};



} // namespace skyland
