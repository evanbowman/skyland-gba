#pragma once

#include "number/int.h"



class Platform;



using BootlegFlashType = u32;


BootlegFlashType bootleg_get_flash_type();


bool bootleg_flash_writeback(BootlegFlashType flash_type,
                             u32 offset,
                             u32 length);


void bootleg_flash_erase(BootlegFlashType flash_type);


void bootleg_cart_init_sram(Platform& pfrm);
