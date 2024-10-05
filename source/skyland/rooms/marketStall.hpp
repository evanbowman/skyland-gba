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
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class MarketStall final : public Decoration
{
public:
    MarketStall(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_market_stall)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        render_exterior(app, buffer);
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        auto p = position();

        buffer[p.x][p.y] = Tile::market_stall_1;
        buffer[p.x + 1][p.y] = Tile::market_stall_2;
        buffer[p.x + 2][p.y] = Tile::market_stall_3;
        buffer[p.x][p.y + 1] = Tile::market_stall_4;
        buffer[p.x + 1][p.y + 1] = Tile::market_stall_5;
        buffer[p.x + 2][p.y + 1] = Tile::market_stall_6;
    }


    void display(Platform::Screen& screen) override
    {
        for (auto& c : characters()) {
            const auto& pos = c->sprite().get_position();
            if (pos.y < 700.0_fixed) {
                auto spr = c->prepare_sprite();
                spr.set_priority(3);
                screen.draw(spr);
            }
        }
    }


    static RoomProperties::Bitmask properties()
    {
        return Decoration::properties() | RoomProperties::not_constructible;
    }


    static const char* name()
    {
        return "market-stall";
    }


    static SystemString ui_name()
    {
        return SystemString::block_market_stall;
    }


    static Vec2<u8> size()
    {
        return {3, 2};
    }
};



} // namespace skyland
