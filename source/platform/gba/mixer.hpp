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

#include "gba_platform_soundcontext.hpp"



static const int audio_buffer_count = 4;


extern AudioBuffer audio_buffers[audio_buffer_count];
extern int audio_front_buffer;
extern volatile bool audio_buffers_consumed[audio_buffer_count];
extern SoundContext snd_ctx;



AudioBuffer* audio_mix();



AudioBuffer* audio_mix_music_only();



void audio_buffer_mixin_sfx(AudioBuffer* mixing_buffer);
