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



class Core : public Room
{
public:
    Core(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;


    void finalize(Platform& pfrm, App& app) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static Float atp_value()
    {
        return 1000.f;
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


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::has_chimney |
            RoomProperties::habitable | RoomProperties::destroy_quietly;
    }
};



class BackupCore : public Core
{
public:
    BackupCore(Island* parent, const RoomCoord& position)
        : Core(parent, position, name())
    {
    }


    Power power_usage(App& app) const override;


    void update(Platform&, App&, Microseconds delta) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Float atp_value()
    {
        return Core::atp_value() - 1;
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


    void render_interior(App& app, TileId buffer[16][16]) override;


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
