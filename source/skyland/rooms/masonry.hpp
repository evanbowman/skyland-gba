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


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_masonry)->c_str();
    }


    void append_name_suffix(Platform& pfrm, StringBuffer<32>& result) override
    {
        switch (gfx_) {
        case 1:
            result += SYSTR(tiled_suffix)->c_str();
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
        return (Decoration::properties() & ~RoomProperties::disallow_chimney) |
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


    virtual ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override
    {
        schedule_repaint();
        gfx_ += 1;
        gfx_ %= 2;
        return null_scene();
    }


private:
    int gfx_ = 0;
};



} // namespace skyland
