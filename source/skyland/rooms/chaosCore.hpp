////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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
               RoomProperties::only_constructible_in_sandbox;
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
