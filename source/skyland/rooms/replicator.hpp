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



class Replicator final : public Room
{
public:
    Replicator(Island* parent, const RoomCoord& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;


    static Vec2<u8> size()
    {
        return {2, 4};
    }


    static const char* name()
    {
        return "replicator";
    }


    static SystemString ui_name()
    {
        return SystemString::block_replicator;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Float atp_value()
    {
        return 800.f;
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


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


    bool create_replicant(Platform& pfrm, App& app) override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::habitable |
               RoomProperties::multiboot_compatible;
    }


private:
    static const auto recharge_time = seconds(5);

    Microseconds recharge_timer_ = 0;
};



} // namespace skyland
