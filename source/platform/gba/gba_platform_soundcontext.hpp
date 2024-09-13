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


#include "memory/buffer.hpp"
#include "string.hpp"


using VolumeScaleLUT = std::array<s8, 256>;


using AudioSample = s8;


struct ActiveSoundInfo
{
    s32 position_;
    const s32 length_;
    const AudioSample* data_;
    s32 priority_;
    const char* name_;
};


struct SoundContext
{
    // Only three sounds will play at a time... hey, sound mixing's expensive!
    Buffer<ActiveSoundInfo, 3> active_sounds;

    const AudioSample* music_track = nullptr;
    StringBuffer<48> music_track_name;
    s32 music_track_length = 0;
    s32 music_track_pos = 0;
};


struct AudioBuffer
{
    static const int sample_count = 280 / 4;

    int samples_[sample_count];
};
