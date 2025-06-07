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
                Character::DrawTransform t;
                t.priority_ = 3;
                c->draw(screen, t);
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
