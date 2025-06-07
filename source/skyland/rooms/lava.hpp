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



class Lava : public Room
{
public:
    Lava(Island* parent, const RoomCoord& position, const char* n = name());


    void set_flood_parent(RoomCoord parent)
    {
        flood_parent_ = parent;
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible | RoomProperties::roof_hidden |
               RoomProperties::fluid | RoomProperties::fragile |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::destroy_quietly |
               RoomProperties::generates_heat |
               RoomProperties::only_constructible_in_sandbox |
               RoomProperties::manufactory_required |
               // lol, fireproof property because the lava just looks strange if
               // it catches on fire.
               RoomProperties::fireproof;
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
        return "lava";
    }


    static SystemString ui_name()
    {
        return SystemString::block_lava;
    }


    bool description_visible() override
    {
        return true;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_lava)->c_str();
    }


    static Icon icon()
    {
        return 2152;
    }


    static Icon unsel_icon()
    {
        return 2168;
    }


    void refresh()
    {
        decay_ = 0;
    }


    virtual void check_flood_parent(Time delta);


protected:
    Time decay_ = 0;

    Time damage_timer_ = 0;

    RoomCoord flood_parent_;
    bool has_flood_parent_ = true;

    Time flood_timer_ = 0;
};



class LavaSource final : public Lava
{
public:
    LavaSource(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void check_flood_parent(Time delta) override;



    static const char* name()
    {
        return "lava-source";
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_lava_source)->c_str();
    }


    static SystemString ui_name()
    {
        return SystemString::block_lava_source;
    }


    static RoomProperties::Bitmask properties()
    {
        return (Lava::properties() & ~RoomProperties::not_constructible) |
               RoomProperties::only_constructible_in_sandbox;
    }
};



} // namespace skyland
