////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "number/int.h"



// SKYLAND stores image files in a specific format. All images occupy 16x16
// pixels. Four bits per pixel.



namespace skyland::img
{



struct PackedPixel
{
    u8 data_;

    u8 first() const
    {
        return data_ & 0x0f;
    }

    u8 second() const
    {
        return (data_ & 0xf0) >> 4;
    }

    void set_first(u8 val)
    {
        data_ &= ~0x0f;
        data_ |= val & 0x0f;
    }

    void set_second(u8 val)
    {
        data_ &= ~0xf0;
        data_ |= (val & 0x0f) << 4;
    }
};



struct Image
{

    static const u8 width = 16;
    static const u8 height = 16;


    u8 get_pixel(u8 x, u8 y) const
    {
        if (x < width and y < height) {
            const u8 adjusted_y = y / 2;
            if (y % 2) {
                return data_[x + adjusted_y * width].first();
            } else {
                return data_[x + adjusted_y * width].second();
            }
        }
        return 0;
    }


    void set_pixel(u8 x, u8 y, u8 value)
    {
        if (x < width and y < height) {
            const u8 adjusted_y = y / 2;
            if (y % 2) {
                data_[x + adjusted_y * width].set_first(value);
            } else {
                data_[x + adjusted_y * width].set_second(value);
            }
        }
    }


    PackedPixel data_[width * height / 2];
};



} // namespace skyland::img
