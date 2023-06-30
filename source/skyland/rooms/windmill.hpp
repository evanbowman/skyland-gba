////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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


#include "decoration.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Windmill final : public Room
{
public:
    Windmill(Island* parent, const RoomCoord& position)
        : Room(parent, name(), position)
    {
    }


    Power power_usage(App& app) const override;


    void finalize(Platform& pfrm, App& app) override;


    int debris_tile() override
    {
        return 2;
    }


    int debris_count() override
    {
        return 2;
    }


    static Category category()
    {
        return Category::power;
    }


    static Float atp_value()
    {
        return 500.f;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_windmill)->c_str();
    }


    u8 tile() const
    {
        return Tile::windmill;
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = tile();
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = tile();
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
    }


    void collect_sprites(Buffer<Sprite, 4>& out) const;


    static const char* name()
    {
        return "windmill";
    }


    static SystemString ui_name()
    {
        return SystemString::block_windmill;
    }


    static RoomProperties::Bitmask properties()
    {
        return Decoration::properties() |
            RoomProperties::disabled_in_tutorials |
            RoomProperties::singleton;
    }


    bool description_visible() override
    {
        return true;
    }


    void display(Platform::Screen& screen, App& app) override;


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 3272;
    }


    static Icon unsel_icon()
    {
        return 3288;
    }

private:
    Fixnum rot_;

    int dup_check_ = 0;
};



} // namespace skyland
