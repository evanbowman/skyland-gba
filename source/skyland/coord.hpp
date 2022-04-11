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

#include "number/numeric.hpp"



namespace skyland
{



using RoomCoord = Vec2<u8>;
using WorldCoord = Vec2<Fixnum>;



struct PackedRoomCoord
{
    PackedRoomCoord()
    {
        data_ = 0;
    }


    PackedRoomCoord(const RoomCoord& c)
    {
        set_x(c.x);
        set_y(c.y);
    }


    u8 x() const
    {
        return data_ & 0x0f;
    }


    u8 y() const
    {
        return data_ & 0xf0;
    }


    void set_x(u8 x)
    {
        data_ &= 0xf0;
        data_ |= 0x0f & x;
    }


    void set_y(u8 y)
    {
        data_ &= 0x0f;
        data_ |= 0xf0 & (y << 4);
    }


    RoomCoord unpack() const
    {
        return RoomCoord{x(), y()};
    }


private:
    u8 data_;
};



}
