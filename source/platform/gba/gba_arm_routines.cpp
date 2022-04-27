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



static constexpr int vram_tile_size()
{
    // 8 x 8 x (4 bitsperpixel / 8 bitsperbyte)
    return 32;
}



// Accepts two vectors of four colors (four colors because indexed 4 bits per
// pixel), and blends the new colors into the existing ones.
static inline u16 blit(u16 prev, u16 replace)
{
    if (replace) {
        if (replace & 0xf000) {
            prev &= 0x0fff;
            prev |= replace & 0xf000;
        }

        if (replace & 0x0f00) {
            prev &= 0xf0ff;
            prev |= replace & 0x0f00;
        }

        if (replace & 0x00f0) {
            prev &= 0xff0f;
            prev |= replace & 0x00f0;
        }

        if (replace & 0x000f) {
            prev &= 0xfff0;
            prev |= replace & 0x000f;
        }
    }

    return prev;
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
