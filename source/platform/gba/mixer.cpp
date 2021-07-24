#include "mixer.hpp"
#include "gba_platform_soundcontext.hpp"



#ifdef __GBA__



extern "C" {
__attribute__((section(".iwram"), long_call)) void
memcpy32(void* dst, const void* src, uint wcount);
void memcpy16(void* dst, const void* src, uint hwcount);
}



AudioBuffer audio_buffers[audio_buffer_count];

int audio_front_buffer;
volatile bool audio_buffers_consumed[audio_buffer_count];


SoundContext snd_ctx;



static void audio_buffer_cp_music(AudioBuffer* mixing_buffer)
{
    if (UNLIKELY(snd_ctx.music_track_pos + AudioBuffer::sample_count >=
                 snd_ctx.music_track_length)) {
        const auto first_batch = snd_ctx.music_track_length - snd_ctx.music_track_pos;
        memcpy32(mixing_buffer->samples_,
                 ((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos,
                 first_batch);

        const auto remaining = AudioBuffer::sample_count - first_batch;
        snd_ctx.music_track_pos = remaining;

        if (remaining) {
            memcpy32(mixing_buffer->samples_ + first_batch,
                     ((u32*)(snd_ctx.music_track)),
                     remaining);
        }

    } else {
        memcpy32(mixing_buffer->samples_,
                 ((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos,
                 AudioBuffer::sample_count);

        snd_ctx.music_track_pos += AudioBuffer::sample_count;
    }
}



static void audio_buffer_mixin_sfx(AudioBuffer* mixing_buffer)
{
    // TODO...
}



void audio_mix_music_only()
{
    auto start = (audio_front_buffer + 1) % audio_buffer_count;
    for (int i = 0; i < 2; ++i) {
        auto index = (start + i) % audio_buffer_count;
        if (audio_buffers_consumed[index]) {
            audio_buffer_cp_music(&audio_buffers[index]);
            audio_buffers_consumed[index] = false;
        }
    }
}



void audio_mix()
{
    auto start = (audio_front_buffer + 1) % audio_buffer_count;
    for (int i = 0; i < 2; ++i) {
        auto index = (start + i) % audio_buffer_count;
        if (audio_buffers_consumed[index]) {
            audio_buffer_cp_music(&audio_buffers[index]);
            audio_buffer_mixin_sfx(&audio_buffers[index]);
            audio_buffers_consumed[index] = false;
        }
    }
}



#endif
