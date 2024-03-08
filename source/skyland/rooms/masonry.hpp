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
