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

    static u8 upsample(u8 channel)
    {
        return (channel * 255 + 15) / 31;
    }

    static u8 downsample(u8 channel)
    {
        return (channel * 31 + 127) / 255;
    }

    inline Color grayscale() const
    {
        const u8 val = (77 * r_ + 150 * g_ + 29 * b_) >> 8;
        return {val, val, val};
    }

    u8 r_;
    u8 g_;
    u8 b_;
};
