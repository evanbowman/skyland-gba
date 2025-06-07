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
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland
{



extern SharedVariable cannon_reload_ms;



class Cannon final : public Weapon
{
public:
    Cannon(Island* parent, const RoomCoord& position);


    void fire() override;
    Time reload_impl() const override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::multiboot_compatible;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "cannon";
    }


    static SystemString ui_name()
    {
        return SystemString::block_cannon;
    }


    static ATP atp_value()
    {
        return 800.0_atp;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 552;
    }


    static Icon unsel_icon()
    {
        return 536;
    }
};


} // namespace skyland
