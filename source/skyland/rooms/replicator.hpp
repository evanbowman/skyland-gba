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



class Replicator final : public Room
{
public:
    Replicator(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static Vec2<u8> size()
    {
        return {2, 3};
    }


    static const char* name()
    {
        return "replicator";
    }


    static SystemString ui_name()
    {
        return SystemString::block_replicator;
    }


    static void format_description(StringBuffer<512>& buffer);


    static ATP atp_value()
    {
        return 800.0_atp;
    }


    static Icon icon()
    {
        return 808;
    }


    static Icon unsel_icon()
    {
        return 792;
    }


    bool non_owner_selectable() const override
    {
        return true;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    bool create_replicant() override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::habitable |
               RoomProperties::multiboot_compatible;
    }


private:
    static const auto recharge_time = seconds(5);
};



} // namespace skyland
