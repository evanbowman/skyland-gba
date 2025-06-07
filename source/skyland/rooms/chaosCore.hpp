////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "core.hpp"



namespace skyland
{



class ChaosCore : public Core
{
public:
    ChaosCore(Island* parent, const RoomCoord& position)
        : Core(parent, position, name())
    {
    }


    Power power_usage() const override;


    static void format_description(StringBuffer<512>& buffer);


    const char* upgrade_mt_name() const override
    {
        return nullptr;
    }


    static ATP atp_value()
    {
        return 1300.0_atp;
    }


    static const char* name()
    {
        return "chaos-core";
    }


    static Vec2<u8> size()
    {
        return {2, 3};
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::has_chimney | RoomProperties::habitable |
               RoomProperties::manufactory_required |
               RoomProperties::goblin_only;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static SystemString ui_name()
    {
        return SystemString::block_chaos_core;
    }


    static Icon icon()
    {
        return 4152;
    }


    static Icon unsel_icon()
    {
        return 4136;
    }


    void finalize() override;
};



} // namespace skyland
