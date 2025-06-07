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


    void render_scaffolding(TileId buffer[16][16]) override
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


    static ATP atp_value()
    {
        return 1.0_atp;
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


    ScenePtr select_impl(const RoomCoord& cursor) override;


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


    void ___rewind___finished_reload() override;


    enum Direction { right, left, up, down };


    virtual bool is_sticky() const;


    ScenePtr setup() override;


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
