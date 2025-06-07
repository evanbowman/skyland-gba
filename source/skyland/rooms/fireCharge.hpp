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



extern SharedVariable fire_charge_reload_ms;



class FireCharge final : public Weapon
{
public:
    FireCharge(Island* parent, const RoomCoord& position);


    void fire() override;
    Time reload_impl() const override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    static Category category()
    {
        return Category::weapon;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::manufactory_required |
               RoomProperties::multiplayer_unsupported;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static const char* name()
    {
        return "fire-charge";
    }


    static SystemString ui_name()
    {
        return SystemString::block_fire_charge;
    }


    static ATP atp_value()
    {
        return 900.0_atp;
    }


    static Icon icon()
    {
        return 2312;
    }


    static Icon unsel_icon()
    {
        return 2328;
    }
};


} // namespace skyland
