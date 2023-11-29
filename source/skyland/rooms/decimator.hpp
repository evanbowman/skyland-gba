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



#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



extern SharedVariable decimator_reload_ms;



class Decimator final : public Room
{
public:
    Decimator(Island* parent, const RoomCoord& position);


    static void format_description(StringBuffer<512>& buffer);


    void update(Microseconds delta) override;


    void rewind(Microseconds delta);


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    void unset_target() override;


    void ___rewind___finished_reload() override;

    void ___rewind___ability_used() override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override;


    static Category category()
    {
        return Category::weapon;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "decimator";
    }


    static SystemString ui_name()
    {
        return SystemString::block_decimator;
    }


    static Icon icon()
    {
        return 1272;
    }


    static Icon unsel_icon()
    {
        return 1288;
    }


    static Float atp_value()
    {
        return 1500.f;
    }


    bool description_visible() override
    {
        return true;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::roof_hidden | RoomProperties::locked_by_default |
               RoomProperties::habitable;
    }


    Microseconds reload_time_remaining() const override
    {
        return reload_;
    }


    void finalize() override;


private:
    int counter_ = 0;


    Microseconds reload_ = 1000 * decimator_reload_ms;
};



} // namespace skyland
