#include "bootleg_cart.hpp"
#include "gba.h"



#define BOOTLEG_FLASH_WRITE(pa, pd) \
    { *(((u16 *)AGB_ROM)+((pa)/2)) = pd; __asm("nop"); }



// BORROWED FROM GOOMBA COLOR EMULATOR SOURCE CODE.
// NOTE FROM THE AUTHOR:
//
// This function will auto-detect four common
// types of reproduction flash cartridges.
// Must run in EWRAM because ROM data is
// not visible to the system while checking.
__attribute__((section(".ewram")))
u32 bootleg_get_flash_type() {

    #define AGB_ROM  ((u8*)0x8000000)

    u32 rom_data, data;
    u16 ie = REG_IE;

    REG_IE = ie & 0xFFFE;

    rom_data = *(u32 *)AGB_ROM;

    // Type 1 or 4
    BOOTLEG_FLASH_WRITE(0, 0xFF);
    BOOTLEG_FLASH_WRITE(0, 0x90);
    data = *(u32 *)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xFF);
    if (rom_data != data) {
        // Check if the chip is responding to this command
        // which then needs a different write command later
        BOOTLEG_FLASH_WRITE(0x59, 0x42);
        data = *(u8 *)(AGB_ROM+0xB2);
        BOOTLEG_FLASH_WRITE(0x59, 0x96);
        BOOTLEG_FLASH_WRITE(0, 0xFF);
        if (data != 0x96) {
            REG_IE = ie;
            return 4;
        }
        REG_IE = ie;
        return 1;
    }

    // Type 2
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
    BOOTLEG_FLASH_WRITE(0x555, 0x56);
    BOOTLEG_FLASH_WRITE(0xAAA, 0x90);
    data = *(u32 *)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    if (rom_data != data) {
        REG_IE = ie;
        return 2;
    }

    // Type 3
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
    BOOTLEG_FLASH_WRITE(0x555, 0x55);
    BOOTLEG_FLASH_WRITE(0xAAA, 0x90);
    data = *(u32 *)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    if (rom_data != data) {
        REG_IE = ie;
        return 3;
    }

    REG_IE = ie;
    return 0;
}
