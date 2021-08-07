////////////////////////////////////////////////////////////////////////////////
//
// All of the code in this file will be compiled as arm code, and placed in the
// IWRAM section of the executable. The system has limited memory for IWRAM
// calls, so limit this file to performace critical code, or code that must be
// defined in IWRAM.
//
////////////////////////////////////////////////////////////////////////////////


#ifdef __GBA__
#define IWRAM_CODE __attribute__((section(".iwram"), long_call))
#else
#define IWRAM_CODE
#endif // __GBA__


#include "gba.h"
#include "mixer.hpp"


int parallax_table[280];
int vertical_parallax_table[280];


extern "C" {
IWRAM_CODE void hblank_full_scroll_isr()
{
    *((volatile short*)0x4000014) = ::parallax_table[(REG_VCOUNT + 1)];
    *((volatile short*)0x4000016) = ::vertical_parallax_table[(REG_VCOUNT + 1)];
}


IWRAM_CODE void hblank_x_scroll_isr()
{
    *((volatile short*)0x4000014) = ::parallax_table[(REG_VCOUNT + 1)];
}



#define REG_SGFIFOA *(volatile u32*)0x40000A0
#define REG_SGFIFOB *(volatile u32*)0x40000A4



static int audio_buffer_read_index = 0;



IWRAM_CODE void audio_update_isr()
{
    REG_SGFIFOA = audio_buffers[audio_front_buffer].samples_[audio_buffer_read_index++];

    if (UNLIKELY(audio_buffer_read_index >= AudioBuffer::sample_count)) {
        audio_buffer_read_index = 0;
        audio_buffers_consumed[audio_front_buffer] = true;
        audio_front_buffer = (audio_front_buffer + 1) % audio_buffer_count;
    }
}



}
