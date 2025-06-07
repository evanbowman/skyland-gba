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



} // namespace skyland
