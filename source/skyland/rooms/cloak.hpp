////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Cloak : public Room
{
public:
    Cloak(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Microseconds delta) override;
    void rewind(Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void render_cloak(TileId buffer[16][16]) override;


    bool cloaks_coordinate(const RoomCoord& c) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::disallow_chimney |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::accepts_ion_damage |
               RoomProperties::manufactory_required;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 500.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "cloak";
    }


    static SystemString ui_name()
    {
        return SystemString::block_cloak;
    }


    static Icon icon()
    {
        return 3832;
    }


    static Icon unsel_icon()
    {
        return 3816;
    }

private:
    int timer_ = 0;
};



} // namespace skyland
