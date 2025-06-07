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


    void update(Time delta) override;


    void rewind(Time delta) override;


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    void unset_target() override;


    void on_level_start() override;


    void ___rewind___finished_reload() override;

    void ___rewind___ability_used() override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override;


    void display(Platform::Screen& screen) override;


    static Category category()
    {
        return Category::weapon;
    }


    Time interval() const;


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


    static ATP atp_value()
    {
        return 1500.0_atp;
    }


    bool description_visible() override
    {
        return true;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::roof_hidden | RoomProperties::goblin_only |
               RoomProperties::habitable;
    }


    Time reload_time_remaining() const override
    {
        return reload_;
    }


    void finalize() override;


    void rewind_projectile_created(int new_counter);
    void rewind_started_firing();


    int counter_ = 0;


    void amplify(bool enable) override;


private:
    Time reload_ = 1000 * decimator_reload_ms;
    u8 flicker_cyc_ = 0;

public:
    bool firing_ = false;
    bool amplify_ = false;
};



} // namespace skyland
