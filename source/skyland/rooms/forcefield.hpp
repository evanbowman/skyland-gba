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



class Forcefield : public Room
{
public:
    Forcefield(Island* parent,
               const RoomCoord& position,
               const char* n = name());


    void update(Time delta) override;
    void rewind(Time delta) override;


    const char* upgrade_mt_name() const override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    void render_scaffolding(TileId buffer[16][16]) override
    {
        // Forcefields float; no scaffolding.
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
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


    static ATP atp_value()
    {
        return 10.0_atp;
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


    static ATP atp_value()
    {
        return 9.0_atp;
    }


    const char* upgrade_mt_name() const override;


    void update(Time delta) override;
    void rewind(Time delta) override;


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
