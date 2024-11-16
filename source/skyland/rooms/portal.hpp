////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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



#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Portal : public Room
{
public:

    Portal(Island* parent, const RoomCoord& position) :
        Room(parent, name(), position)
    {
    }


    // static void format_description(StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::portal_1;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::wall_plain_1;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override
    {
        matrix[position().x][position().y] = true;
    }


    static ATP atp_value()
    {
        return 70.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "portal";
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::manufactory_required |
               RoomProperties::multiboot_compatible;
    }


    static SystemString ui_name()
    {
        return SystemString::block_portal;
    }


    static Icon icon()
    {
        return 4120;
    }


    static Icon unsel_icon()
    {
        return 4104;
    }
};



}
