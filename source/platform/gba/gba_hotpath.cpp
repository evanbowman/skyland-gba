////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



// Ripped from tonc demo. This code is decent already, no need to write code
// for drawing a circle.
IWRAM_CODE
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



extern Buffer<const char*, 4> completed_sounds_buffer;
extern volatile bool completed_sounds_lock;



#define REG_SGFIFOA *(volatile u32*)0x40000A0


// NOTE: The primary audio mixing routine.
IWRAM_CODE
void audio_update_fast_isr()
{
    alignas(4) AudioSample mixing_buffer[8];

    auto& music_pos = snd_ctx.music_track_pos;
    const auto music_len = snd_ctx.music_track_length;


    if (music_pos > music_len) {
        music_pos = 0;
    }
    // Load 8 music samples upfront (in chunks of four), to try to take
    // advantage of sequential cartridge reads.
    auto music_in = (u32*)mixing_buffer;
    *(music_in++) = ((u32*)(snd_ctx.music_track))[music_pos++];
    *(music_in) = ((u32*)(snd_ctx.music_track))[music_pos++];

    auto it = snd_ctx.active_sounds.begin();
    while (it not_eq snd_ctx.active_sounds.end()) {

        // Cache the position index into the sound data, then pre-increment by
        // eight. Incrementing by eight upfront is better than checking if index
        // + 8 is greater than sound length and then performing index += 8 after
        // doing the mixing, saves an addition.
        int pos = it->position_;
        it->position_ += 8;

        // Aha! __builtin_expect actually results in measurably better latency
        // for once!
        if (UNLIKELY(it->position_ >= it->length_)) {
            if (not completed_sounds_lock) {
                completed_sounds_buffer.push_back(it->name_);
            }
            it = snd_ctx.active_sounds.erase(it);
            if (snd_ctx.active_sounds.empty()) {
            }
        } else {
            // Manually unrolled loop below. Better performance during testing,
            // uses more iwram of course.
            //
            // Note: storing the mixing buffer in a pointer and incrementing the
            // write location resulted in notably better performance than
            // subscript indexing into mixing_buffer with literal indices
            // (mixing_buffer[0], mixing_buffer[1], etc.).
            AudioSample* out = mixing_buffer;
            *(out++) += it->data_[pos++]; // 0
            *(out++) += it->data_[pos++]; // 1
            *(out++) += it->data_[pos++]; // 2
            *(out++) += it->data_[pos++]; // 3
            *(out++) += it->data_[pos++]; // 4
            *(out++) += it->data_[pos++]; // 5
            *(out++) += it->data_[pos++]; // 6
            *(out) += it->data_[pos++];   // 7
            ++it;
        }
    }

    auto sound_out = (u32*)mixing_buffer;

    // NOTE: yeah the register is a FIFO
    REG_SGFIFOA = *(sound_out++);
    REG_SGFIFOA = *(sound_out);
}



static constexpr int vram_tile_size()
{
    // 8 x 8 x (4 bitsperpixel / 8 bitsperbyte)
    return 32;
}



// Accepts two vectors of four colors (indexed 4bpp).
static inline u32 blit(u32 current_color, u32 add_color)
{
    u32 result = 0;

    if (add_color & 0xf0000000) {
        result |= add_color & 0xf0000000;
    } else {
        result |= current_color & 0xf0000000;
    }

    if (add_color & 0x0f000000) {
        result |= add_color & 0x0f000000;
    } else {
        result |= current_color & 0x0f000000;
    }

    if (add_color & 0x00f00000) {
        result |= add_color & 0x00f00000;
    } else {
        result |= current_color & 0x00f00000;
    }

    if (add_color & 0x000f0000) {
        result |= add_color & 0x000f0000;
    } else {
        result |= current_color & 0x000f0000;
    }

    if (add_color & 0x0000f000) {
        result |= add_color & 0x0000f000;
    } else {
        result |= current_color & 0x0000f000;
    }

    if (add_color & 0x00000f00) {
        result |= add_color & 0x00000f00;
    } else {
        result |= current_color & 0x00000f00;
    }

    if (add_color & 0x000000f0) {
        result |= add_color & 0x000000f0;
    } else {
        result |= current_color & 0x000000f0;
    }

    if (add_color & 0x0000000f) {
        result |= add_color & 0x0000000f;
    } else {
        result |= current_color & 0x0000000f;
    }

    return result;
}



IWRAM_CODE
void blit_tile(u16* out, u16* in)
{
    auto out32 = (u32*)out;
    auto in32 = (u32*)in;

    for (int i = 0; i < vram_tile_size() / 4; ++i) {

        auto val = *in32;
        auto prev = *out32;

        *out32 = blit(prev, val);

        ++out32;
        ++in32;
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
