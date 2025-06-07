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
#include "skyland/tile.hpp"



namespace skyland
{



class Barrier final : public Room
{
public:
    Barrier(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void apply_damage(Health damage, const DamageConfiguration& conf) override;


    static const char* name()
    {
        return "barrier";
    }


    static SystemString ui_name()
    {
        return SystemString::block_barrier;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_barrier)->c_str();
    }


    static ATP atp_value()
    {
        return 0.00001_atp;
    }


    bool description_visible() override
    {
        return true;
    }


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::only_constructible_in_sandbox |
               RoomProperties::fireproof;
    }


    static Icon icon()
    {
        return 2088;
    }


    static Icon unsel_icon()
    {
        return 2104;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;
};



} // namespace skyland
