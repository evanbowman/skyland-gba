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
                             BasicCharacter* for_character) override
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
                auto spr = c->prepare_sprite();
                spr.set_priority(3);
                screen.draw(spr);
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


    ScenePtr<Scene> select()
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
