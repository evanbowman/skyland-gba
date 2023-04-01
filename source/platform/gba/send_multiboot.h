#pragma once


#include "number/int.h"



#ifdef __cplusplus
extern "C" {
#endif


enum MbResult
{
    mb_result_failure,
    mb_result_success,
};



#define MB_DEFAULT_TRIES 200000



enum MbResult mb_send_rom(u16* begin,
                          u16* end,
                          int tries);



#ifdef __cplusplus
}	   // extern "C"
#endif
