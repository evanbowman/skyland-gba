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
