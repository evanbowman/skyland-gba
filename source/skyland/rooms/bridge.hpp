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
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void show_island_interior(Island* island);



class Bridge final : public Decoration
{
public:
    Bridge(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        matrix[position().x][position().y] = true;
        matrix[position().x + 1][position().y] = true;
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::bridge;
        buffer[position().x + 1][position().y] = InteriorTile::bridge;
        if (buffer[position().x][position().y + 1] == 0) {
            buffer[position().x][position().y + 1] = Tile::bridge_truss;
        }
        if (buffer[position().x + 1][position().y + 1] == 0) {
            buffer[position().x + 1][position().y + 1] = Tile::bridge_truss;
        }
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::bridge;
        buffer[position().x + 1][position().y] = Tile::bridge;
        if (buffer[position().x][position().y + 1] == 0) {
            buffer[position().x][position().y + 1] = Tile::bridge_truss;
        }
        if (buffer[position().x + 1][position().y + 1] == 0) {
            buffer[position().x + 1][position().y + 1] = Tile::bridge_truss;
        }
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


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override
    {
    }



    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() & ~RoomProperties::roof_hidden) |
               RoomProperties::habitable;
    }


    ScenePtr select()
    {
        // Unlike most rooms, the bridge shows inhabitants while viewing a
        // castle's exterior. If selecting a character, we want to show the
        // interior representation of the castle.
        if (not characters().empty() and not parent()->interior_visible()) {

            show_island_interior(parent());
        }

        return Room::do_select();
    }


    static const char* name()
    {
        return "bridge";
    }


    static SystemString ui_name()
    {
        return SystemString::block_bridge;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static Icon icon()
    {
        return 1544;
    }


    static Icon unsel_icon()
    {
        return 1560;
    }
};



} // namespace skyland
