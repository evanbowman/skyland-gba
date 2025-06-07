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

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class PoweredHull final : public Room
{
public:
    PoweredHull(Island* parent, const RoomCoord& position);


    void update(Time delta) override;
    void rewind(Time delta) override;


    TileId tile() const;


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


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 2.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "energized-hull";
    }


    static SystemString ui_name()
    {
        return SystemString::block_energized_hull;
    }


    static Icon icon()
    {
        return 1208;
    }


    static Icon unsel_icon()
    {
        return 1224;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::roof_hidden |
               RoomProperties::accepts_ion_damage |
               RoomProperties::multiboot_compatible;
    }

private:
    TileId last_tile_;
};



} // namespace skyland
