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



class BeamGun final : public Weapon
{
public:
    BeamGun(Island* parent, const RoomCoord& position);


    void fire() override;
    Time reload_impl() const override;


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::multiboot_compatible |
               RoomProperties::only_constructible_in_sandbox;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {3, 1};
    }


    static const char* name()
    {
        return "beam-gun";
    }


    static SystemString ui_name()
    {
        return SystemString::block_beam_gun;
    }


    static ATP atp_value()
    {
        return 1100.0_atp;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 2808;
    }


    static Icon unsel_icon()
    {
        return 2792;
    }
};


} // namespace skyland
