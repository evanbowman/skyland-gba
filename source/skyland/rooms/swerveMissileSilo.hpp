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


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland
{



class SwerveMissileSilo final : public Weapon
{
public:
    SwerveMissileSilo(Island* parent, const RoomCoord& position);


    void fire() override;
    Time reload_impl() const override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::vertical;
    }


    static void format_description(StringBuffer<512>& buffer);


    ScenePtr select_impl(const RoomCoord& cursor) override;


    static Category category()
    {
        return Category::weapon;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "swerve-missile";
    }


    static SystemString ui_name()
    {
        return SystemString::block_homing_missile;
    }


    static ATP atp_value()
    {
        return 800.0_atp;
    }


    static Icon icon()
    {
        return 584;
    }


    static Icon unsel_icon()
    {
        return 568;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::only_constructible_in_sandbox |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::multiplayer_unsupported;
    }


    void plot_walkable_zones(bool matrix[16][16], Character*) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    struct Node
    {
        struct Pos
        {
            u8 x : 4;
            u8 y : 4;
        };
        Pos pos_;
        bool near_;
    };

    using Path = Buffer<Node, 5>;
    using PathArray = Node[Path::capacity()];

    void set_path(const Path& p);


private:
    PathArray path_;
};


} // namespace skyland
