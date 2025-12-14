////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
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



class ParticleLance final : public Room
{
public:
    ParticleLance(Island* parent, const RoomCoord& position);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    ScenePtr select_impl(const RoomCoord& cursor) override;


    void update(Time delta) override;
    void rewind(Time delta) override;


    void display(Platform::Screen&) override;


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    static void format_description(StringBuffer<512>& buffer);


    void project_damage(Health damage);


    static Category category()
    {
        return Category::weapon;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::multiboot_compatible |
               RoomProperties::oversize_explosion |
               RoomProperties::only_constructible_in_sandbox;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {3, 1};
    }


    void finalize() override;


    static constexpr const char* name()
    {
        return "particle-lance";
    }


    static SystemString ui_name()
    {
        return SystemString::block_particle_lance;
    }


    static ATP atp_value()
    {
        return 2000.0_atp;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 4344;
    }


    void on_salvage() override;


    void on_destroy();


    static Icon unsel_icon()
    {
        return 4328;
    }

    Health dmg_count_ = 0;
    bool active_ = false;

private:
    Time timer_ = 0;
    u8 flicker_cyc_ = 0;
};


} // namespace skyland
