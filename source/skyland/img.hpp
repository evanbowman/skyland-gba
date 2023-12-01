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
