#pragma once


#include "number/int.h"



#ifdef __cplusplus
extern "C" {
#endif


enum MbResult
{
 mb_result_success,
 mb_result_falure,
};



enum MBResult mb_send_rom(u16* begin, u16* end);



#ifdef __cplusplus
}	   // extern "C"
#endif
