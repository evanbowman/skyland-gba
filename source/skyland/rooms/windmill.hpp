////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


    void update(Microseconds delta) override;


    void rewind(Microseconds delta) override;


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override
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
    bool parity_ = false;
};



} // namespace skyland
