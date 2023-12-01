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



class LadyLiberty final : public Decoration
{
public:
    LadyLiberty(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_lady_liberty)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::liberty_1;
        buffer[position().x + 1][position().y] = InteriorTile::liberty_2;
        buffer[position().x][position().y + 1] = InteriorTile::liberty_3;
        buffer[position().x + 1][position().y + 1] = InteriorTile::liberty_4;
        buffer[position().x][position().y + 2] = InteriorTile::liberty_5;
        buffer[position().x + 1][position().y + 2] = InteriorTile::liberty_6;
        buffer[position().x][position().y + 3] = InteriorTile::liberty_7;
        buffer[position().x + 1][position().y + 3] = InteriorTile::liberty_8;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::liberty_1;
        buffer[position().x + 1][position().y] = Tile::liberty_2;
        buffer[position().x][position().y + 1] = Tile::liberty_3;
        buffer[position().x + 1][position().y + 1] = Tile::liberty_4;
        buffer[position().x][position().y + 2] = Tile::liberty_5;
        buffer[position().x + 1][position().y + 2] = Tile::liberty_6;
        buffer[position().x][position().y + 3] = Tile::liberty_7;
        buffer[position().x + 1][position().y + 3] = Tile::liberty_8;
    }


    static const char* name()
    {
        return "lady-liberty";
    }


    static SystemString ui_name()
    {
        return SystemString::block_lady_liberty;
    }


    static Vec2<u8> size()
    {
        return {2, 4};
    }


    static Icon icon()
    {
        return 2056;
    }


    static Icon unsel_icon()
    {
        return 2072;
    }
};



} // namespace skyland
