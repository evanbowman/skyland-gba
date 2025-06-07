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



class Masonry final : public Decoration
{
public:
    Masonry(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    int debris_tile() override
    {
        return 2;
    }


    int debris_count() override
    {
        return 2;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_masonry)->c_str();
    }


    void append_name_suffix(StringBuffer<32>& result) override
    {
        switch (gfx_) {
        case 1:
            result += SYSTR(tiled_suffix)->c_str();
            break;

        case 2:
            result += SYSTR(vines_suffix)->c_str();
            break;

        case 3:
            result += SYSTR(brick_suffix)->c_str();
            break;
        }
        return;
    }


    u8 tile() const
    {
        switch (gfx_) {
        case 0:
            return Tile::masonry;

        default:
        case 1:
            return Tile::tile;

        case 2:
            return Tile::masonry_vines;

        case 3:
            return Tile::masonry_brick;
        }
    }


    lisp::Value* serialize() override
    {
        lisp::ListBuilder builder;

        builder.push_back(L_SYM(name()));
        builder.push_back(L_INT(position().x));
        builder.push_back(L_INT(position().y));

        builder.push_back(L_INT(gfx_));

        if (health() not_eq max_health()) {
            builder.push_back(lisp::make_integer(health()));
        }

        return builder.result();
    }



    void deserialize(lisp::Value* list) override
    {
        auto c = lisp::get_list(list, 3);
        if (c->type() == lisp::Value::Type::integer) {
            gfx_ = c->integer().value_;
        }

        if (lisp::length(list) >= 5) {
            __set_health(lisp::get_list(list, 4)->integer().value_);
        }
    }



    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = tile();
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = tile();
    }


    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() & ~RoomProperties::disallow_chimney &
                ~RoomProperties::multiplayer_unsupported) |
               RoomProperties::fireproof | RoomProperties::multiboot_compatible;
    }


    static const char* name()
    {
        return "masonry";
    }


    static SystemString ui_name()
    {
        return SystemString::block_masonry;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1448;
    }


    static Icon unsel_icon()
    {
        return 1464;
    }


    void set_gfx(int gfx)
    {
        gfx_ = gfx;
    }


    virtual ScenePtr select_impl(const RoomCoord& cursor) override
    {
        schedule_repaint();
        update_description();
        gfx_ += 1;
        gfx_ %= 4;
        return null_scene();
    }


private:
    int gfx_ = 0;
};



} // namespace skyland
