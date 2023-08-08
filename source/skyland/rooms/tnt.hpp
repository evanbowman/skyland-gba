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
#include "skyland/systemString.hpp"



namespace skyland
{



class Explosive : public Room
{
public:
    Explosive(Island* parent,
              const RoomCoord& position,
              const char* class_name = name());


    void finalize(Platform& pfrm, App& app) override;


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void render_scaffolding(App& app, TileId buffer[16][16]) override
    {
    }


    bool description_visible() override
    {
        return true;
    }


    void plot_walkable_zones(App& app,
                             bool matrix[16][16],
                             BasicCharacter* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::locked_by_default |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::highly_flammable |
               RoomProperties::multiboot_compatible;
    }


    static Float atp_value()
    {
        return 2000.f;
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


    void apply_damage(Platform& pfrm, App& app, Health damage) override;


    void ignite(Platform& pfrm,
                App& app,
                int range,
                Health damage,
                bool spread_fire);


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const RoomCoord& cursor) override;


protected:
    bool ignition_ = false;
    Microseconds damage_timer_ = 0;
};



class TNT final : public Explosive
{
public:
    TNT(Island* parent, const RoomCoord& position)
        : Explosive(parent, position, name())
    {
    }


    void finalize(Platform& pfrm, App& app) override
    {
        Room::finalize(pfrm, app);

        if (not ignition_) {
            return;
        } else {
            ignite(pfrm, app, 2, 200, true);
        }
    }


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
                          App& app,
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


    void finalize(Platform& pfrm, App& app) override
    {
        Room::finalize(pfrm, app);

        if (not ignition_) {
            return;
        } else {
            ignite(pfrm, app, 2, 100, true);
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


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override
    {
        return Room::select(pfrm, app, cursor);
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const RoomCoord& cursor) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;
};



} // namespace skyland
