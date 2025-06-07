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



class Infirmary final : public Room
{
public:
    Infirmary(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "infirmary";
    }


    static SystemString ui_name()
    {
        return SystemString::block_infirmary;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable | RoomProperties::multiboot_compatible;
    }


    static ATP atp_value()
    {
        return 800.0_atp;
    }


    static Icon icon()
    {
        return 680;
    }


    static Icon unsel_icon()
    {
        return 664;
    }


    void finalize() override;


    void on_powerchange() override;


    bool allows_powerdown() override;


    void amplify(bool enabled) override;


    Time heal_interval() const;


    void display(Platform::Screen& screen) override;


private:
    Time heal_timer_ = 0;
    bool amplify_ = false;
    u8 anim_cyc_ = 0;
};



} // namespace skyland
