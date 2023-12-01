////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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
