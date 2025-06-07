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



class Bulkhead final : public Room
{
public:
    Bulkhead(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override;


    static void format_description(StringBuffer<512>& buffer);


    void ___rewind___finished_reload() override;


    static ATP atp_value()
    {
        return 20.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "bulkhead-door";
    }


    static SystemString ui_name()
    {
        return SystemString::block_bulkhead_door;
    }


    static Icon icon()
    {
        return 648;
    }


    static Icon unsel_icon()
    {
        return 632;
    }


    void set_open(bool open);


    void on_powerchange() override;


    bool allows_powerdown() override;


    bool is_open() const
    {
        return open_;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable |
               RoomProperties::multiboot_compatible | RoomProperties::fireproof;
    }


private:
    bool open_ = true;
    bool interior_visible_ = false;
};



} // namespace skyland
