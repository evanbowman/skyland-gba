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

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Balloon : public Room
{
public:
    Balloon(Island* parent, const RoomCoord& position)
        : Room(parent, name(), position)
    {
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        matrix[position().x + 1][position().y + 4] = true;
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        auto pos = position();
        buffer[pos.x][pos.y] = Tile::balloon_1;
        buffer[pos.x + 1][pos.y] = Tile::balloon_2;
        buffer[pos.x + 2][pos.y] = Tile::balloon_3;
        buffer[pos.x][pos.y + 1] = Tile::balloon_4;
        buffer[pos.x + 1][pos.y + 1] = Tile::balloon_5;
        buffer[pos.x + 2][pos.y + 1] = Tile::balloon_6;
        buffer[pos.x][pos.y + 2] = Tile::balloon_7;
        buffer[pos.x + 1][pos.y + 2] = Tile::balloon_8;
        buffer[pos.x + 2][pos.y + 2] = Tile::balloon_9;
        buffer[pos.x + 1][pos.y + 3] = Tile::basket_1;
        buffer[pos.x][pos.y + 4] = Tile::basket_2;
        buffer[pos.x + 1][pos.y + 4] = Tile::basket_3;
        buffer[pos.x + 2][pos.y + 4] = Tile::basket_4;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        render_interior(app, buffer);
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::fragile |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::habitable | RoomProperties::not_constructible;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1.0_atp;
    }


    static const char* name()
    {
        return "balloon";
    }


    static SystemString ui_name()
    {
        return SystemString::block_balloon;
    }


    static Vec2<u8> size()
    {
        return {3, 5};
    }


    static Icon icon()
    {
        return 3960;
    }


    static Icon unsel_icon()
    {
        return 3944;
    }


    static Category category()
    {
        return Category::power;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_balloon)->c_str();
    }


    void display(Platform::Screen& screen) override
    {
        for (auto& c : characters()) {
            auto pos = c->sprite().get_position();
            if (pos.y < 700.0_fixed) {
                Character::DrawTransform t;
                t.y_displace_ = Fixnum::from_integer(-3);
                t.priority_ = 3;
                c->draw(screen, t);
            }
        }
    }
};



} // namespace skyland
