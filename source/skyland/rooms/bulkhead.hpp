////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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


    void update(Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override;


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
