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



class IonFizzler final : public Room
{
public:
    IonFizzler(Island* parent, const RoomCoord& position);


    void update(Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float atp_value()
    {
        return 10.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "ion-fizzler";
    }


    static SystemString ui_name()
    {
        return SystemString::block_ion_fizzler;
    }


    static Icon icon()
    {
        return 1240;
    }


    static Icon unsel_icon()
    {
        return 1256;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::accepts_ion_damage |
               RoomProperties::cancels_ion_damage | RoomProperties::habitable;
    }
};



} // namespace skyland
