#pragma once

#include "number/int.h"



class Platform;



using BootlegFlashType = u32;


BootlegFlashType bootleg_get_flash_type();


void bootleg_flash_write(BootlegFlashType flash_type);


void bootleg_cart_init_sram(Platform& pfrm);
