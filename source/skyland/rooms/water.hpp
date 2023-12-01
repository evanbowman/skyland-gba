////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Water : public Room
{
public:
    Water(Island* parent, const RoomCoord& position, const char* n = name());


    void set_flood_parent(RoomCoord parent)
    {
        flood_parent_ = parent;
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void update(Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible | RoomProperties::roof_hidden |
               RoomProperties::fluid | RoomProperties::fragile |
               RoomProperties::destroy_quietly | RoomProperties::fireproof;
    }


    static ATP atp_value()
    {
        return 0.1_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "water";
    }


    static SystemString ui_name()
    {
        return SystemString::block_water;
    }


    bool description_visible() override
    {
        return true;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_water)->c_str();
    }


    static Icon icon()
    {
        return 2120;
    }


    static Icon unsel_icon()
    {
        return 2136;
    }


    void refresh()
    {
        decay_ = 0;
    }


    virtual void check_flood_parent(Microseconds delta);


protected:
    Microseconds decay_ = 0;

    RoomCoord flood_parent_;
    bool has_flood_parent_ = true;

    Microseconds flood_timer_ = 0;
};



class WaterSource final : public Water
{
public:
    WaterSource(Island* parent, const RoomCoord& position);


    void update(Microseconds delta) override;


    void check_flood_parent(Microseconds delta) override;


    static SystemString ui_name()
    {
        return SystemString::block_water_source;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_water_source)->c_str();
    }


    static const char* name()
    {
        return "water-source";
    }


    static RoomProperties::Bitmask properties()
    {
        return Water::properties() & ~RoomProperties::not_constructible;
    }
};



} // namespace skyland
