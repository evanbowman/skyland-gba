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

#include "hull.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class BronzeHull final : public Hull
{
public:
    BronzeHull(Island* parent, const RoomCoord& position);


    using Hull::Hull;


    int debris_tile() override
    {
        return 2;
    }


    static const char* name()
    {
        return "bronze-hull";
    }


    static SystemString ui_name()
    {
        return SystemString::block_bronze_hull;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_bronze_hull)->c_str();
    }


    static RoomProperties::Bitmask properties()
    {
        return Hull::properties() | RoomProperties::disabled_in_tutorials |
               RoomProperties::locked_by_default |
               RoomProperties::multiboot_compatible;
    }


    static Icon icon()
    {
        return 1624;
    }


    static Icon unsel_icon()
    {
        return 1640;
    }


    TileId tile() const;


    void update(Time delta) override;
    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;
};



} // namespace skyland
