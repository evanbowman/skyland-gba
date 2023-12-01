////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
