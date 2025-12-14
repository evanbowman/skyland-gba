////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
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



class OverdriveCore : public Room
{
public:
    OverdriveCore(Island* parent,
                  const RoomCoord& position,
                  const char* n = name());


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static ATP atp_value()
    {
        return 1100.0_atp;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const constexpr char* name()
    {
        return "overdrive-core";
    }


    static SystemString ui_name()
    {
        return SystemString::block_overdrive_core;
    }


    static Icon icon()
    {
        return 4312;
    }


    static Icon unsel_icon()
    {
        return 4296;
    }


    void apply_damage(Health damage, const DamageConfiguration& conf) override;


    static Health overload_threshold(Health max_hp);


    bool is_overloaded() const;


    void update(Time delta) override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::has_chimney |
               RoomProperties::habitable | RoomProperties::oversize_explosion |
               RoomProperties::multiboot_compatible |
               RoomProperties::not_constructible;
    }


    Optional<UpgradeList> upgrade_mt_list() const override;

};



} // namespace skyland
