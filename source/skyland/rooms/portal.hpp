////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    Portal(Island* parent, const RoomCoord& position)
        : Room(parent, name(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_portal)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::portal_1;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::wall_plain_1;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
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



} // namespace skyland
