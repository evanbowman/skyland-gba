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


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Torch : public Decoration
{
public:
    Torch(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    void burn_damage(Platform& pfrm, App& app, Health damage) override
    {
        // Torches take no burn damage (that's kinda the whole point).
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_torch)->c_str();
    }


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
        Room::render_scaffolding(app, buffer);
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::torch;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::torch;
    }


    static u32 properties()
    {
        return (Decoration::properties() &
                ~(RoomProperties::disallow_chimney |
                  RoomProperties::locked_by_default)) |
               RoomProperties::highly_flammable |
               RoomProperties::sandbox_mode_only;
    }


    static const char* name()
    {
        return "torch";
    }


    static SystemString ui_name()
    {
        return SystemString::block_torch;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 2376;
    }


    static Icon unsel_icon()
    {
        return 2392;
    }
};



} // namespace skyland
