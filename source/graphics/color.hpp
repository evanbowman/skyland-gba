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

#include "number/numeric.hpp"


// Color representation is often platform specific, e.g., OpenGL uses RGBA,
// Nintendo GameBoy uses 15 bit BGR (5 bits per channel). For the sake of
// portability, the colors are described only with an enum, and the
// implementations of the Platform header are responsible for using the correct
// values.
enum class ColorConstant {
    null,
    // clang-format off
    electric_blue     = 0x00FFFF,
    turquoise_blue    = 0x00FFDD,
    cerulean_blue     = 0x66E0FF,
    picton_blue       = 0x4DACFF,
    maya_blue         = 0x4FBCFF,
    steel_blue        = 0x345680,
    spanish_crimson   = 0xE81858,
    aerospace_orange  = 0xFD5200,
    safety_orange     = 0xFC7500,
    rich_black        = 0x000010,
    stil_de_grain     = 0xF9DC5C,
    silver_white      = 0xF4F4F8,
    aged_paper        = 0xDEC397,
    green             = 0x27C8AF,
    med_blue_gray     = 0x7674A9,
    indigo_tint       = 0x255F85,
    // clang-format on
};


constexpr inline ColorConstant custom_color(int hex)
{
    return static_cast<ColorConstant>(hex);
}


constexpr inline ColorConstant custom_color(u8 r, u8 g, u8 b)
{
    return static_cast<ColorConstant>((r << 16) | (g << 8) | b);
}


struct ColorMix
{
    ColorConstant color_ = ColorConstant::null;
    u8 amount_ = 0;
};
