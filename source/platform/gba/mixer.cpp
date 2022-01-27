#include "mixer.hpp"
#include "gba_platform_soundcontext.hpp"



#ifdef __GBA__



extern "C" {
__attribute__((section(".iwram"), long_call)) void
memcpy32(void* dst, const void* src, uint wcount);
void memcpy16(void* dst, const void* src, uint hwcount);
}



SoundContext snd_ctx;



// NOTE: I got rid of this version of the mixer.



#endif
