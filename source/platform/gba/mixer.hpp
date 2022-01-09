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
