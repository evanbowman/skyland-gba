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
#include "skyland/systemString.hpp"



namespace skyland
{



class Radiator final : public Room
{
public:
    Radiator(Island* parent, const RoomCoord& position);


    static void format_description(StringBuffer<512>& buffer);


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }



    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    // static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::workshop_required |
               RoomProperties::locked_by_default |
               RoomProperties::generates_heat |
               RoomProperties::disabled_in_tutorials;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "radiator";
    }


    static SystemString ui_name()
    {
        return SystemString::block_radiator;
    }


    static Icon icon()
    {
        return 1832;
    }


    static Icon unsel_icon()
    {
        return 1848;
    }


    bool opponent_display_on_hover() const override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


    using ChrBuffer = Buffer<Character*, 16>;


    void collect_nearby_chrs(ChrBuffer& output);


private:
    void emit_radiation();


    Time damage_timer_ = 0;
};



} // namespace skyland
