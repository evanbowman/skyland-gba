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
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland
{



class SparkCannon final : public Room
{
public:
    SparkCannon(Island* parent, const RoomCoord& position);


    static int default_health()
    {
        return 200;
    }


    static int default_cost()
    {
        return 2500;
    }


    static int default_power()
    {
        return 30;
    }


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void on_lightning() override;
    void on_lightning_rewind() override;


    void ___rewind___finished_reload() override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::locked_by_default |
               RoomProperties::manufactory_required;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static const char* name()
    {
        return "spark-cannon";
    }


    static SystemString ui_name()
    {
        return SystemString::block_spark_cannon;
    }


    static Float atp_value()
    {
        return 900.f;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 3672;
    }


    static Icon unsel_icon()
    {
        return 3656;
    }

    ScenePtr<Scene> select(const RoomCoord& cursor) override;

    Microseconds reload_time_remaining() const override
    {
        return seconds(level_) - 1;
    }

    u8 level() const
    {
        return level_;
    }

private:
    u8 level_ = 0;
};


} // namespace skyland
