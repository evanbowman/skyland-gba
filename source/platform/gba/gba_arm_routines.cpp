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


////////////////////////////////////////////////////////////////////////////////
//
// All of the code in this file will be compiled as arm code, and placed in the
// IWRAM section of the executable. The system has limited memory for IWRAM
// calls, so limit this file to performace critical code, or code that must be
// defined in IWRAM.
//
////////////////////////////////////////////////////////////////////////////////



#include "gba.h"
#include "mixer.hpp"


s16 parallax_table[280];
s16 vertical_parallax_table[280];



// Ripped from tonc demo. This code is decent already, no need to write my code
// for drawing a circle.
void win_circle(u16 winh[], int x0, int y0, int rr)
{
#define IN_RANGE(x, min, max) (((x) >= (min)) && ((x) < (max)))

    int x = 0, y = rr, d = 1 - rr;
    u32 tmp;

    // u32 col = 0;
    // CpuFastSet(&col, winh, 160 | (1 << 24));
    // memset16(winh, 0, 160);

    while (y >= x) {
        // Side octs
        tmp = clamp(x0 + y, 0, 240);
        tmp += clamp(x0 - y, 0, 240) << 8;

        if (IN_RANGE(y0 - x, 0, 160)) // o4, o7
            winh[y0 - x] = tmp;
        if (IN_RANGE(y0 + x, 0, 160)) // o0, o3
            winh[y0 + x] = tmp;

        // Change in y: top/bottom octs
        if (d >= 0) {
            tmp = clamp(x0 + x, 0, 240);
            tmp += clamp(x0 - x, 0, 240) << 8;

            if (IN_RANGE(y0 - y, 0, 160)) // o5, o6
                winh[y0 - y] = tmp;
            if (IN_RANGE(y0 + y, 0, 160)) // o1, o2
                winh[y0 + y] = tmp;

            d -= 2 * (--y);
        }
        d += 2 * (x++) + 3;
    }
    winh[160] = winh[0];
}



static constexpr int vram_tile_size()
{
    // 8 x 8 x (4 bitsperpixel / 8 bitsperbyte)
    return 32;
}



// Accepts two vectors of four colors (indexed 4bpp).
static inline u16 blit(u16 current_color, u16 add_color)
{
    u16 result = 0;

    if (add_color & 0xf000) {
        result |= add_color & 0xf000;
    } else {
        result |= current_color & 0xf000;
    }

    if (add_color & 0x0f00) {
        result |= add_color & 0x0f00;
    } else {
        result |= current_color & 0x0f00;
    }

    if (add_color & 0x00f0) {
        result |= add_color & 0x00f0;
    } else {
        result |= current_color & 0x00f0;
    }

    if (add_color & 0x000f) {
        result |= add_color & 0x000f;
    } else {
        result |= current_color & 0x000f;
    }

    return result;
}



IWRAM_CODE
void blit_tile(u16* out, u16* in)
{
    for (int i = 0; i < vram_tile_size() / 2; ++i) {

        auto val = *in;
        auto prev = *out;

        *out = blit(prev, val);

        ++out;
        ++in;
    }
}


//
// In case you're wondering why I'm not using a better blit function with a
// pre-generated mask, I actually already tried it. It's not faster enough than
// the above code to be worth wasting rom space on a mask image. Only marginally
// faster, doesn't make graphical artifacts go away. Rather than optimizing
// raster more than I already have, I'd prefer to focus on eliminating draw
// calls in the first place.
//
// IWRAM_CODE
// void _blit_tile(u16* out, const u16* in, const u16* in_mask)
// {
//     for (int i = 0; i < vram_tile_size() / 2; ++i) {
//
//         auto mask = *in_mask;
//
//         u16 result = 0;
//         result |= *out & ~mask;
//         result |= *in & mask;
//         *out = result;
//
//         ++out;
//         ++in;
//         ++in_mask;
//     }
// }
