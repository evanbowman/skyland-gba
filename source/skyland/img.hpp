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
