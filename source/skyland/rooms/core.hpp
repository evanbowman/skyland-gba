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



class Core : public Room
{
public:
    Core(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void finalize() override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static ATP atp_value()
    {
        return 1000.0_atp;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "power-core";
    }


    static SystemString ui_name()
    {
        return SystemString::block_power_core;
    }


    static Icon icon()
    {
        return 744;
    }


    static Icon unsel_icon()
    {
        return 728;
    }


    const char* upgrade_mt_name() const override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::has_chimney |
               RoomProperties::habitable | RoomProperties::destroy_quietly |
               RoomProperties::multiboot_compatible;
    }
};



class BackupCore : public Core
{
public:
    BackupCore(Island* parent, const RoomCoord& position)
        : Core(parent, position, name())
    {
    }


    Power power_usage() const override;


    void update(Time delta) override;


    static void format_description(StringBuffer<512>& buffer);


    const char* upgrade_mt_name() const override;


    static ATP atp_value()
    {
        return Core::atp_value() - 1.0_atp;
    }


    static const char* name()
    {
        return "backup-core";
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::has_chimney |
               RoomProperties::habitable |
               RoomProperties::disabled_in_tutorials;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    static SystemString ui_name()
    {
        return SystemString::block_backup_core;
    }


    static Icon icon()
    {
        return 3592;
    }


    static Icon unsel_icon()
    {
        return 3576;
    }
};



} // namespace skyland
