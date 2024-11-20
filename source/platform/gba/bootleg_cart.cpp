#include "bootleg_cart.hpp"
#include "critical_section.hpp"
#include "filesystem.hpp"
#include "gba.h"
#include "platform/conf.hpp"
#include "platform/platform.hpp"



// FIXME: I think bootleg carts actually support up to 64 * 1024 bytes of sram.
#define BOOTLEG_SRAM ((u8*)0xE000000)



static u32 flash_sram_area = 0;



#define BOOTLEG_FLASH_WRITE(pa, pd)                                            \
    {                                                                          \
        *(((u16*)AGB_ROM) + ((pa) / 2)) = pd;                                  \
        __asm("nop");                                                          \
    }



// BORROWED FROM GOOMBA COLOR EMULATOR SOURCE CODE.
// NOTE FROM THE AUTHOR:
//
// This function will auto-detect four common
// types of reproduction flash cartridges.
// Must run in EWRAM because ROM data is
// not visible to the system while checking.
__attribute__((section(".ewram"))) u32 bootleg_get_flash_type()
{

#define AGB_ROM ((u8*)0x8000000)

    u32 rom_data, data;
    u16 ie = REG_IE;

    REG_IE = ie & 0xFFFE;

    rom_data = *(u32*)AGB_ROM;

    // Type 1 or 4
    BOOTLEG_FLASH_WRITE(0, 0xFF);
    BOOTLEG_FLASH_WRITE(0, 0x90);
    data = *(u32*)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xFF);
    if (rom_data != data) {
        // Check if the chip is responding to this command
        // which then needs a different write command later
        BOOTLEG_FLASH_WRITE(0x59, 0x42);
        data = *(u8*)(AGB_ROM + 0xB2);
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
    data = *(u32*)AGB_ROM;
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
    data = *(u32*)AGB_ROM;
    BOOTLEG_FLASH_WRITE(0, 0xF0);
    if (rom_data != data) {
        REG_IE = ie;
        return 3;
    }

    REG_IE = ie;
    return 0;
}



__attribute__((section(".ewram"))) void
bootleg_flash_erase_impl(BootlegFlashType flash_type)
{
    if (flash_sram_area == 0) {
        return;
    }
    u32 sa = flash_sram_area;

    if (flash_type == 0)
        return;

    if (flash_type == 1) {
        // Erase flash sector
        BOOTLEG_FLASH_WRITE(sa, 0xFF);
        BOOTLEG_FLASH_WRITE(sa, 0x60);
        BOOTLEG_FLASH_WRITE(sa, 0xD0);
        BOOTLEG_FLASH_WRITE(sa, 0x20);
        BOOTLEG_FLASH_WRITE(sa, 0xD0);
        while (1) {
            __asm("nop");
            if (*(((u16*)AGB_ROM) + (sa / 2)) == 0x80) {
                break;
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xFF);

    } else if (flash_type == 2) {
        // Erase flash sector
        BOOTLEG_FLASH_WRITE(sa, 0xF0);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
        BOOTLEG_FLASH_WRITE(0x555, 0x56);
        BOOTLEG_FLASH_WRITE(0xAAA, 0x80);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
        BOOTLEG_FLASH_WRITE(0x555, 0x56);
        BOOTLEG_FLASH_WRITE(sa, 0x30);
        while (1) {
            __asm("nop");
            if (*(((u16*)AGB_ROM) + (sa / 2)) == 0xFFFF) {
                break;
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);

    } else if (flash_type == 3) {
        // Erase flash sector
        BOOTLEG_FLASH_WRITE(sa, 0xF0);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
        BOOTLEG_FLASH_WRITE(0x555, 0x55);
        BOOTLEG_FLASH_WRITE(0xAAA, 0x80);
        BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
        BOOTLEG_FLASH_WRITE(0x555, 0x55);
        BOOTLEG_FLASH_WRITE(sa, 0x30);
        while (1) {
            __asm("nop");
            if (*(((u16*)AGB_ROM) + (sa / 2)) == 0xFFFF) {
                break;
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);
    }
}



void bootleg_flash_erase(BootlegFlashType flash_type)
{
    auto ctx = critical_section_enter();
    bootleg_flash_erase_impl(flash_type);
    critical_section_exit(ctx);
}



// This function will issue a flash sector erase
// operation at the given sector address and then
// write 64 kilobytes of SRAM data to Flash ROM.
// Must run in EWRAM because ROM data is
// not visible to the system while erasing/writing.
__attribute__((section(".ewram"))) void
bootleg_flash_writeback_impl(BootlegFlashType flash_type,
                             u32 offset,
                             u32 length)
{
    if (flash_sram_area == 0) {
        return;
    }
    u32 sa = flash_sram_area;

    if (flash_type == 0)
        return;

    if (flash_type == 1) {

        // Write data
        for (u32 i = offset; i < offset + length; i += 2) {
            BOOTLEG_FLASH_WRITE(sa + i, 0x40);
            BOOTLEG_FLASH_WRITE(sa + i,
                                (*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                                    (*(u8*)(BOOTLEG_SRAM + i)));
            while (1) {
                __asm("nop");
                if (*(((u16*)AGB_ROM) + (sa / 2)) == 0x80) {
                    break;
                }
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xFF);

    } else if (flash_type == 2) {

        BOOTLEG_FLASH_WRITE(sa, 0xF0);

        // Write data
        for (u32 i = offset; i < offset + length; i += 2) {
            BOOTLEG_FLASH_WRITE(0xAAA, 0xA9);
            BOOTLEG_FLASH_WRITE(0x555, 0x56);
            BOOTLEG_FLASH_WRITE(0xAAA, 0xA0);
            BOOTLEG_FLASH_WRITE(sa + i,
                                (*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                                    (*(u8*)(BOOTLEG_SRAM + i)));
            while (1) {
                __asm("nop");
                if (*(((u16*)AGB_ROM) + ((sa + i) / 2)) ==
                    ((*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                     (*(u8*)(BOOTLEG_SRAM + i)))) {
                    break;
                }
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);

    } else if (flash_type == 3) {

        // Write data
        for (u32 i = offset; i < offset + length; i += 2) {
            BOOTLEG_FLASH_WRITE(0xAAA, 0xAA);
            BOOTLEG_FLASH_WRITE(0x555, 0x55);
            BOOTLEG_FLASH_WRITE(0xAAA, 0xA0);
            BOOTLEG_FLASH_WRITE(sa + i,
                                (*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                                    (*(u8*)(BOOTLEG_SRAM + i)));
            while (1) {
                __asm("nop");
                if (*(((u16*)AGB_ROM) + ((sa + i) / 2)) ==
                    ((*(u8*)(BOOTLEG_SRAM + i + 1)) << 8 |
                     (*(u8*)(BOOTLEG_SRAM + i)))) {
                    break;
                }
            }
        }
        BOOTLEG_FLASH_WRITE(sa, 0xF0);
    }
}



bool bootleg_flash_writeback(BootlegFlashType flash_type,
                             u32 offset,
                             u32 length)
{
    auto ctx = critical_section_enter();
    bootleg_flash_writeback_impl(flash_type, offset, length);
    critical_section_exit(ctx);

    // Byte verify:

    auto cmp1 = (u8*)BOOTLEG_SRAM;
    auto cmp2 = (u8*)AGB_ROM + flash_sram_area;

    cmp1 += offset;
    cmp2 += offset;

    for (u32 i = 0; i < length; ++i) {
        if (*cmp1 not_eq *cmp2) {
            return false;
        }
    }

    return true;
}

#pragma GCC diagnostic ignored "-Warray-bounds"

extern char __rom_end__;
extern int save_capacity;


static void bytecopy(u8* dest, u8* src, u32 size)
{
    while (size--) {
        *(dest++) = *(src++);
    }
}



void bootleg_cart_init_sram()
{
    const u32 total_rom_size =
        u32(&__rom_end__ - 0x8000000) + filesystem::size();
    u32 flash_size = 0;
    flash_sram_area = 0;

    Conf conf;
    flash_sram_area = conf.expect<Conf::Integer>("hardware.gameboy_advance",
                                                 "repro_flash_save_address");

    info(format("rom size: %kb", total_rom_size / 1000));

    // Determine the size of the flash chip by checking for ROM loops,
    // then set the SRAM storage area 0x40000 bytes before the end.
    // This is due to different sector sizes of different flash chips,
    // and should hopefully cover all cases.
    if (memcmp(AGB_ROM + 4, AGB_ROM + 4 + 0x400000, 0x40) == 0) {
        flash_size = 0x400000;
    } else if (memcmp(AGB_ROM + 4, AGB_ROM + 4 + 0x800000, 0x40) == 0) {
        flash_size = 0x800000;
    } else if (memcmp(AGB_ROM + 4, AGB_ROM + 4 + 0x1000000, 0x40) == 0) {
        flash_size = 0x1000000;
    } else {
        flash_size = 0x2000000;
    }
    info(format("flash size detected: %kb", flash_size / 1000));

    if (flash_sram_area == 0) {
        flash_sram_area = flash_size - 0x40000;
    }

    // RIP if the selected storage area is within the Goomba Color ROM...
    if (total_rom_size > flash_sram_area) {
        flash_sram_area = 0;
        info("ROM too large to allocate repro flash sram area!");
        return;
    }

    // Finally, restore the SRAM data and proceed.
    bytecopy(BOOTLEG_SRAM, ((u8*)AGB_ROM + flash_sram_area), save_capacity);

    info("Restored SRAM from repro flash.");
}
