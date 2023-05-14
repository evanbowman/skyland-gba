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
#include "script/listBuilder.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Statue final : public Decoration
{
public:
    Statue(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        switch (gfx_) {
        default:
            buffer[position().x][position().y] = InteriorTile::statue_1;
            buffer[position().x][position().y + 1] = InteriorTile::statue_2;
            break;

        case 1:
            buffer[position().x][position().y] = InteriorTile::statue_3;
            buffer[position().x][position().y + 1] = InteriorTile::statue_4;
            break;

        case 2:
            buffer[position().x][position().y] = InteriorTile::statue_5;
            buffer[position().x][position().y + 1] = InteriorTile::statue_6;
            break;

        case 3:
            buffer[position().x][position().y] = InteriorTile::statue_7;
            buffer[position().x][position().y + 1] = InteriorTile::statue_8;
            break;
        }
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        render_interior(app, buffer);
    }


    void append_name_suffix(Platform& pfrm, StringBuffer<32>& result) override
    {
        switch (gfx_) {
        case 1:
            result += SYSTR(goodboy_suffix)->c_str();
            break;

        case 2:
            result += SYSTR(pkmn_suffix)->c_str();
            break;

        case 3:
            result += SYSTR(troll_suffix)->c_str();
            break;
        }
        return;
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


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_statue)->c_str();
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


    static const char* name()
    {
        return "statue";
    }


    static SystemString ui_name()
    {
        return SystemString::block_statue;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static Icon icon()
    {
        return 1480;
    }


    static Icon unsel_icon()
    {
        return 1496;
    }


    virtual ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override
    {
        schedule_repaint();
        gfx_ += 1;
        gfx_ %= 4;
        update_description();
        return null_scene();
    }


private:
    int gfx_ = 0;
};



} // namespace skyland
