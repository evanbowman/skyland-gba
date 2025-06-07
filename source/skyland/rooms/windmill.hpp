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


    Power power_usage() const override;


    void finalize() override;


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


    static ATP atp_value()
    {
        return 500.0_atp;
    }


    static void format_description(StringBuffer<512>& buffer)
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


    void update(Time delta) override;


    void rewind(Time delta) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
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


    void display(Platform::Screen& screen) override;


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
