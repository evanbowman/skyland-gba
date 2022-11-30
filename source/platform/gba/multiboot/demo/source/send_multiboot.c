#include <stdint.h>
#include <stdio.h>


/* #include "incbin.h" */
/* INCBIN(Skyland_MB_ROM, "SkylandClient_mb.gba"); */



#define REG_BASE 0x04000000
#define REG_SIOCNT		*(volatile u16*)(REG_BASE + 0x128)
#define REG_RCNT		*(volatile u16*)(REG_BASE + 0x134)
#define R_MULTI			0x0000
#define SIO_115200		0x0003
#define SIO_MULTI		0x2000

#define REG_SIOMLT_SEND	*(volatile u16*)(REG_BASE + 0x12a)
#define REG_SIOMULTI1	*(volatile u16*)(REG_BASE + 0x122)
#define SIO_START		(1<<7)

typedef	struct
{
	u32	reserved1[5];
	u8	handshake_data;
	u8	padding;
	u16	handshake_timeout;
	u8	probe_count;
	u8	client_data[3];
	u8	palette_data;
	u8	response_bit;
	u8	client_bit;
	u8	reserved2;
	u8	*boot_srcp;
	u8	*boot_endp;
	u8	*masterp;
	u8	*reserved3[3];
	u32	system_work2[4];
	u8	sendflag;
	u8	probe_target_bit;
	u8	check_wait;
	u8	server_type;
} MultiBootParam;


int MultiBoot(MultiBootParam* mb, u32 mode);


void mb_debug(u16 color)
{
    *(unsigned int*)0x04000000 = 0x0403;

    ((unsigned short*)0x06000000)[120+80*240] = color;// 0x001F
    ((unsigned short*)0x06000000)[121+80*240] = color;// 0x03E0
    ((unsigned short*)0x06000000)[120+81*240] = color;// 0x7C00
    ((unsigned short*)0x06000000)[121+81*240] = color;// 0x7C00
    ((unsigned short*)0x06000000)[122+81*240] = color;// 0x7C00
    ((unsigned short*)0x06000000)[122+80*240] = color;// 0x7C00
    ((unsigned short*)0x06000000)[122+82*240] = color;// 0x7C00
    ((unsigned short*)0x06000000)[120+82*240] = color;// 0x7C00
    ((unsigned short*)0x06000000)[121+82*240] = color;// 0x7C00
}


u16 rgb15(int red, int green, int blue)
{
    return red + (green<<5) + (blue<<10);
}



typedef enum LogLevel {
	LOG_FATAL                   = 0x100,
	LOG_ERR                     = 0x101,
	LOG_WARN                    = 0x102,
	LOG_INFO                    = 0x103
} LogLevel;


#define REG_LOG_ENABLE          *(volatile u16*) 0x4FFF780
#define REG_LOG_LEVEL           *(volatile u16*) 0x4FFF700

#include <stdarg.h>


static void mgba_log(const char* msg)
{
    *(volatile uint16_t*)(0x4FFF780) = 0xC0DE;

    char* reg_debug_string = (char*)(0x4FFF600);
    int characters_left = strlen(msg);

    while (characters_left > 0) {
        volatile u16* reg_debug_flags = (u16*)(0x4FFF700);

        int characters_to_write = characters_left;
        __builtin_memcpy(reg_debug_string, msg, characters_to_write);
        *reg_debug_flags = 2 | 0x100;
        msg += characters_to_write;
        characters_left -= characters_to_write;
    }
}



void log_int(int val)
{
    char buffer[30];
    sprintf(buffer, "%d", val);
    mgba_log(buffer);
}



u16 mb_exchange(u16 value)
{
    while (REG_SIOCNT & SIO_START) ;
    REG_SIOMLT_SEND = value;
    REG_SIOCNT = REG_SIOCNT | SIO_START;
    while (REG_SIOCNT & SIO_START) ;
    u16 result = REG_SIOMULTI1;
    mgba_log("out");
    log_int(value);
    mgba_log("in");
    log_int(result);
    return result;
}



enum MBResult mb_send_rom(u16* begin, u16* end)
{
    MultiBootParam mp;

    while (((u8*)end - (u8*)begin) < 0x100 + 0xc0) {
        ++end;
    }

    if (((u8*)end - (u8*)begin) > 0x3ffff) {
        return mb_result_falure;
    }

    while (((u8*)end - (u8*)begin) % 0x10 != 0) {
        // Round up transfer size to multiple of 0x10
        end = (u16*)((u8*)end + 1);
    }


    mgba_log("payload size");
    log_int(((u8*)end - (u8*)begin)); // 2160


    REG_RCNT = R_MULTI;
    REG_SIOCNT = SIO_MULTI | SIO_115200;

    u16 resp;
 RESTART:
    mb_debug(0xffff);

    resp = 0xff;
    do { // Poll until other gba recognized.
        resp = mb_exchange(0x6200);
    } while (resp != 0);

    mb_debug(0x001F);

    int client_connected[3] = {0}; // FIXME: detect client id rather than
                                   // hardcoding bit 2.

    for (int i = 0; i < 15; ++i) {
        resp = mb_exchange(0x6200);
        if ((resp & 0xfff0) == 0x7200) {
            switch (resp & 0xf) {
            case 1:
                client_connected[0] = 1;
                break;

            case 2:
                client_connected[1] = 1;
                break;

            case 4:
                client_connected[2] = 1;
                break;
            }
            break;
        } else {
            // TODO: sleep?
            goto RESTART;
        }
    }

    mb_debug(rgb15(0, 31, 0)); // (green)

    if (mb_exchange(0x6102) != 0x7202) {
        return mb_result_falure;
    }

    mb_debug(rgb15(31, 31, 0)); // (yellow)

    u16* header_out = begin;
    for (int i = 0; i < 0xc0; i += 2) {
        resp = mb_exchange(*(header_out++));
    }

    if (mb_exchange(0x6200) != 0x0002) {
        return mb_result_falure;
    }

    if (mb_exchange(0x6202) != 0x7202) {
        return mb_result_falure;
    }

    mb_debug(rgb15(0, 0, 31)); // (blue)

    u8 handshake_data = 0;
    u8 client_data = 0;
    while (1) { // send palette data, wait for resp == 0x73nn rather than 0x72nn
        resp = mb_exchange(0x6393);
        if (resp >> 8 == 0x73) {
            client_data = resp & 0xff;
            break;
        }
    }

    handshake_data = (0x11 + (int)client_data + 0xff + (int)0xff) % 256;

    mb_debug(rgb15(31, 15, 0)); // (orange)

    if (mb_exchange(0x6400 | (u8)handshake_data) >> 8 != 0x73) {
        return mb_result_falure;
    }

    mb_debug(rgb15(15, 15, 15)); // (gray)

    // ready for transfer!

    mgba_log("calculated handshake");
    log_int(handshake_data); // 27
    mgba_log("client data");
    log_int(client_data); // 12

    mp.palette_data = 0x93;
    mp.handshake_data = (u8)handshake_data;
    mp.client_data[0] = client_data;
    mp.client_data[1] = 0xff;
    mp.client_data[2] = 0xff;
    mp.client_bit = 2;
    mp.boot_srcp = (u8*)begin + 0xc0;
    mp.boot_endp = (u8*)end;

    mgba_log("srcp");
    log_int((intptr_t)mp.boot_srcp);
    mgba_log("endp");
    log_int((intptr_t)mp.boot_endp);
    mgba_log("len");
    log_int((intptr_t)mp.boot_endp - (intptr_t)mp.boot_srcp);

    resp = MultiBoot(&mp, 1);
    if (resp == 1) {
        mb_debug(rgb15(0, 0, 0));
    } else {
        mb_debug(rgb15(0, 31, 31));
    }

    return mb_result_success;
}
