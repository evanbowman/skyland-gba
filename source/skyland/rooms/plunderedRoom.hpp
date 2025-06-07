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



class PlunderedRoom final : public Room
{
public:
    PlunderedRoom(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 10.0_atp;
    }


    static RoomCoord size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "plundered-room";
    }


    static SystemString ui_name()
    {
        return SystemString::block_plundered_room;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::only_constructible_in_sandbox |
               RoomProperties::habitable | RoomProperties::multiboot_compatible;
    }
};



} // namespace skyland
