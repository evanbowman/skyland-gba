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



namespace skyland
{



class Switch : public Decoration
{
public:
    Switch(Island* parent, const RoomCoord& position);


    static const char* name()
    {
        return "switch";
    }


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    ScenePtr<Scene> select(const RoomCoord& cursor) override;


    virtual lisp::Value* serialize() override;
    virtual void deserialize(lisp::Value*) override;


    static void format_description(StringBuffer<512>& buffer);


    bool description_visible() override
    {
        return true;
    }


    static SystemString ui_name()
    {
        return SystemString::block_switch;
    }


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::locked_by_default | RoomProperties::fragile;
    }


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;

    RoomCoord branch_1_;
    RoomCoord branch_2_;

    bool setup_ = false;

private:
    bool on_ = true;
};



} // namespace skyland
