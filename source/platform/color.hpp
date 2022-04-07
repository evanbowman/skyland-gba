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

#include "graphics/color.hpp"
#include "number/numeric.hpp"


class Color
{
public:
    Color(ColorConstant k)
    {
        const auto val = static_cast<int>(k);
        r_ = (val & 0xFF0000) >> 16;
        g_ = (val & 0x00FF00) >> 8;
        b_ = (val & 0x0000FF);

        // The gba uses 5-bit color.
        r_ >>= 3;
        g_ >>= 3;
        b_ >>= 3;
    }

    ColorConstant hex() const
    {
        auto r = r_ << 3;
        auto g = g_ << 3;
        auto b = b_ << 3;

        return static_cast<ColorConstant>(r << 16 | g << 8 | b);
    }

    Color(u8 r, u8 g, u8 b) : r_(r), g_(g), b_(b)
    {
    }

    inline u16 bgr_hex_555() const
    {
        return (r_) + ((g_) << 5) + ((b_) << 10);
    }

    inline u16 rgb_hex_555() const
    {
        return (r_ << 10) + ((g_) << 5) + ((b_) << 5);
    }

    static Color from_bgr_hex_555(u16 val)
    {
        return {
            u8(0x1F & val), u8((0x3E0 & val) >> 5), u8((0x7C00 & val) >> 10)};
    }

    inline Color invert() const
    {
        constexpr u8 max{31};
        return {u8(max - r_), u8(max - g_), u8(max - b_)};
    }

    inline Color grayscale() const
    {
        const u8 val = 0.3f * r_ + 0.59f * g_ + 0.11f * b_;
        return {val, val, val};
    }

    // Convert color channel to number out of 255 rather than out of 31.
    static u8 upsample(u8 channel)
    {
        return (255.f / 31.f) * channel;
    }

    // Convert color channel to number out of 31 rather than out of 255.
    static u8 downsample(u8 channel)
    {
        return (31.f / 255.f) * channel;
    }

    u8 r_;
    u8 g_;
    u8 b_;
};
