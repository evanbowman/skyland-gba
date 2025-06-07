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



class Torch final : public Decoration
{
public:
    Torch(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void burn_damage(Health damage) override
    {
        // Torches take no burn damage (that's kinda the whole point).
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_torch)->c_str();
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
        Room::render_scaffolding(buffer);
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::torch;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::torch;
    }


    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() &
                ~(RoomProperties::disallow_chimney |
                  RoomProperties::locked_by_default)) |
               RoomProperties::flag_mount | RoomProperties::highly_flammable |
               RoomProperties::only_constructible_in_sandbox;
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
