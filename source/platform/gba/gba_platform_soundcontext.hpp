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


#include "memory/buffer.hpp"


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
    s32 music_track_length = 0;
    s32 music_track_pos = 0;
};


struct AudioBuffer
{
    static const int sample_count = 280 / 4;

    int samples_[sample_count];
};
