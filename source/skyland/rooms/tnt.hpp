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



class Explosive : public Room
{
public:
    Explosive(Island* parent,
              const RoomCoord& position,
              const char* class_name = name());


    void finalize() override;


    void update(Time delta) override;
    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    bool description_visible() override
    {
        return true;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::salvage_disallowed |
               RoomProperties::locked_by_default |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::highly_flammable |
               RoomProperties::multiboot_compatible |
               RoomProperties::oversize_explosion;
    }


    static ATP atp_value()
    {
        return 2000.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "dynamite";
    }


    static SystemString ui_name()
    {
        return SystemString::block_dynamite_1;
    }


    static Icon icon()
    {
        return 1672;
    }


    static Icon unsel_icon()
    {
        return 1688;
    }


    void apply_damage(Health damage, const DamageConfiguration& conf) override;


    void ignite(int range, Health damage, bool spread_fire);


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


protected:
    bool ignition_ = false;
    Time damage_timer_ = 0;
};



class TNT final : public Explosive
{
public:
    TNT(Island* parent, const RoomCoord& position)
        : Explosive(parent, position, name())
    {
    }


    void finalize() override;


    static RoomProperties::Bitmask properties()
    {
        return Explosive::properties() | RoomProperties::manufactory_required |
               RoomProperties::locked_by_default;
    }


    static const char* name()
    {
        return "dynamite-ii";
    }


    static SystemString ui_name()
    {
        return SystemString::block_dynamite_2;
    }


    static Icon icon()
    {
        return 1704;
    }


    static Icon unsel_icon()
    {
        return 1720;
    }


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;
};



class Cesium : public Explosive
{
public:
    Cesium(Island* parent, const RoomCoord& position)
        : Explosive(parent, position, name())
    {
    }


    void finalize() override
    {
        Room::finalize();

        if (not ignition_) {
            return;
        } else {
            ignite(2, 100, true);
        }
    }


    static RoomProperties::Bitmask properties()
    {
        return (Explosive::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::only_constructible_in_sandbox;
    }


    static const char* name()
    {
        return "cesium";
    }


    static SystemString ui_name()
    {
        return SystemString::block_cesium;
    }


    static Icon icon()
    {
        return 2408;
    }


    static Icon unsel_icon()
    {
        return 2424;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override
    {
        return Room::select_impl(cursor);
    }


    static void format_description(StringBuffer<512>& buffer);


    void update(Time delta) override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;
};



} // namespace skyland
