////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    void append_name_suffix(StringBuffer<32>& result) override
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


    static void format_description(StringBuffer<512>& buffer)
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


    virtual ScenePtr<Scene> select_impl(const RoomCoord& cursor) override
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
