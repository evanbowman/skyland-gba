////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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
