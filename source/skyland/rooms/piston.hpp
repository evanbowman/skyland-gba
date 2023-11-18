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

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Piston : public Room
{
public:
    Piston(Island* parent, const RoomCoord& position, const char* n = name());


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void render_scaffolding(App& app, TileId buffer[16][16]) override
    {
    }


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |

               RoomProperties::multiplayer_unsupported |
               RoomProperties::disabled_in_tutorials;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float atp_value()
    {
        return 1.f;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "piston";
    }


    static SystemString ui_name()
    {
        return SystemString::block_piston;
    }


    ScenePtr<Scene> select(App& app, const RoomCoord& cursor) override;


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


    void ___rewind___finished_reload(App&) override;


    enum Direction { right, left, up, down };


    virtual bool is_sticky() const;


    ScenePtr<Scene> setup(App&) override;


    void set_direction(Direction dir)
    {
        dir_ = dir;
    }


private:
    bool opened_ = false;
    Direction dir_ = Direction::right;
};



class StickyPiston : public Piston
{
public:
    StickyPiston(Island* parent, const RoomCoord& position)
        : Piston(parent, position, name())
    {
    }


    static const char* name()
    {
        return "sticky-piston";
    }


    virtual bool is_sticky() const
    {
        return true;
    }


    static SystemString ui_name()
    {
        return SystemString::block_sticky_piston;
    }
};



} // namespace skyland
