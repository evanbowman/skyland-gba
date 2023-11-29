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



class Forcefield : public Room
{
public:
    Forcefield(Island* parent,
               const RoomCoord& position,
               const char* n = name());


    void update(Microseconds delta) override;
    void rewind(Microseconds delta) override;


    const char* upgrade_mt_name() const override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    void render_scaffolding(TileId buffer[16][16])
    {
        // Forcefields float; no scaffolding.
    }


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(StringBuffer<512>& buffer);


    TileId tile() const;


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
        return {1, 1};
    }


    static const char* name()
    {
        return "forcefield";
    }


    static SystemString ui_name()
    {
        return SystemString::block_forcefield;
    }


    static Icon icon()
    {
        return 712;
    }


    static Icon unsel_icon()
    {
        return 696;
    }


    int debris_tile() override
    {
        return 3;
    }


    void finalize() override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required |
               RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::accepts_ion_damage | RoomProperties::fireproof |
               RoomProperties::destroy_quietly |
               RoomProperties::multiboot_compatible;
    }

protected:
    TileId last_tile_;
};



class Forcefield2 : public Forcefield
{
public:
    Forcefield2(Island* parent, const RoomCoord& position)
        : Forcefield(parent, position, name())
    {
    }


    static SystemString ui_name()
    {
        return SystemString::block_forcefield2;
    }


    static const char* name()
    {
        return "forcefield*";
    }


    static Float atp_value()
    {
        return 9.f;
    }


    const char* upgrade_mt_name() const override;


    void update(Microseconds delta) override;
    void rewind(Microseconds delta) override;


    static void format_description(StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static RoomProperties::Bitmask properties()
    {
        return (Forcefield::properties() |
                RoomProperties::manufactory_required) &
               ~RoomProperties::workshop_required;
    }


    static Icon icon()
    {
        return 2248;
    }


    static Icon unsel_icon()
    {
        return 2264;
    }


    TileId tile() const;
};



} // namespace skyland
