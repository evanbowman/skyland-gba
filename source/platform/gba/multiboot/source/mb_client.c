//
// An implementation of client-side rendering for a multiboot program. Mainly,
// calculations happen in the host gba, and this program just receives network
// packets and renders stuff. The entire program needs to fit in GBA RAM, so
// this code is intentionally minimal.
//


#include "../../gba.h"
#include "charblock.h"
#include <stdbool.h>
#include <stddef.h>


#define VRAM_TILE_SIZE 32


static int multiplayer_is_master()
{
    return (REG_SIOCNT & (1 << 2)) == 0 && (REG_SIOCNT & (1 << 3));
}


static int multiplayer_error()
{
    return REG_SIOCNT & (1 << 6);
}


static bool multiplayer_validate_modes()
{
    // 1 if all GBAs are in the correct mode, 0 otherwise.
    return REG_SIOCNT & (1 << 3);
}


static bool multiplayer_validate()
{
    if (!multiplayer_validate_modes()) {
        return false;
    } else {
    }
    return true;
}


#define MESSAGE_ITERS (6 / sizeof(u16))


typedef struct WireMessage
{
    u16 data_[MESSAGE_ITERS];
    struct WireMessage* next_;
} WireMessage;


typedef WireMessage TxInfo;
typedef WireMessage RxInfo;


#define PACKET_POOL_SIZE 32
WireMessage packet_pool_data[PACKET_POOL_SIZE];
WireMessage* packet_pool;



void packet_pool_init()
{
    packet_pool = NULL;

    for (int i = 0; i < PACKET_POOL_SIZE; ++i) {
        WireMessage* current = packet_pool_data + i;
        current->next_ = packet_pool;
        packet_pool = current;
    }
}



WireMessage* packet_pool_alloc()
{
    if (packet_pool) {
        WireMessage* result = packet_pool;
        packet_pool = result->next_;
        return result;
    } else {
        return NULL;
    }
}



void packet_pool_free(WireMessage* packet)
{
    packet->next_ = packet_pool;
    packet_pool = packet;
}



struct MultiplayerComms
{
#define TX_RING_SIZE 8
#define RX_RING_SIZE 64

    int tx_ring_write_pos;
    int tx_ring_read_pos;
    TxInfo* tx_ring[TX_RING_SIZE];

    int rx_ring_write_pos;
    int rx_ring_read_pos;
    RxInfo* rx_ring[RX_RING_SIZE];

    int rx_iter_state;
    RxInfo* rx_current_message;

    int tx_iter_state;
    TxInfo* tx_current_message;

    int null_bytes_written;

    RxInfo* poller_current_message;

    bool rx_current_all_zeroes;
} mc;



static TxInfo* tx_ring_pop()
{
    TxInfo* msg = NULL;

    for (int i = mc.tx_ring_read_pos; i < mc.tx_ring_read_pos + TX_RING_SIZE;
         ++i) {
        int index = i % TX_RING_SIZE;
        if (mc.tx_ring[index]) {
            msg = mc.tx_ring[index];
            mc.tx_ring[index] = NULL;
            mc.tx_ring_read_pos = index;
            return msg;
        }
    }

    mc.tx_ring_read_pos += 1;
    mc.tx_ring_read_pos %= TX_RING_SIZE;

    // The transmit ring is completely empty!
    return NULL;
}


static void rx_ring_push(RxInfo* message)
{
    if (mc.rx_ring[mc.rx_ring_write_pos]) {
        // The reader does not seem to be keeping up!
        RxInfo* lost_message = mc.rx_ring[mc.rx_ring_write_pos];

        mc.rx_ring[mc.rx_ring_write_pos] = NULL;
        packet_pool_free(lost_message);
    }

    mc.rx_ring[mc.rx_ring_write_pos] = message;
    mc.rx_ring_write_pos += 1;
    mc.rx_ring_write_pos %= RX_RING_SIZE;
}


static RxInfo* rx_ring_pop()
{
    RxInfo* msg = NULL;

    for (int i = mc.rx_ring_read_pos; i < mc.rx_ring_read_pos + RX_RING_SIZE;
         ++i) {
        int index = i % RX_RING_SIZE;

        if (mc.rx_ring[index]) {
            msg = mc.rx_ring[index];
            mc.rx_ring[index] = NULL;
            mc.rx_ring_read_pos = index;

            return msg;
        }
    }

    mc.rx_ring_read_pos += 1;
    mc.rx_ring_read_pos %= RX_RING_SIZE;

    return NULL;
}


static void multiplayer_rx_receive()
{
    if (mc.rx_iter_state == MESSAGE_ITERS) {
        if (mc.rx_current_message) {
            if (mc.rx_current_all_zeroes) {
                packet_pool_free(mc.rx_current_message);
            } else {
                rx_ring_push(mc.rx_current_message);
            }
        }

        mc.rx_current_all_zeroes = true;

        mc.rx_current_message = packet_pool_alloc();
        if (!mc.rx_current_message) {
            // message loss...
        }
        mc.rx_iter_state = 0;
    }

    if (mc.rx_current_message) {
        const u16 val =
            multiplayer_is_master() ? REG_SIOMULTI1 : REG_SIOMULTI0;
        if (mc.rx_current_all_zeroes && val) {
            mc.rx_current_all_zeroes = false;
        }
        mc.rx_current_message->data_[mc.rx_iter_state++] = val;

    } else {
        mc.rx_iter_state++;
    }
}



static void multiplayer_tx_send()
{
    if (mc.tx_iter_state == MESSAGE_ITERS) {
        if (mc.tx_current_message) {
            packet_pool_free(mc.tx_current_message);
        }
        mc.tx_current_message = tx_ring_pop();
        mc.tx_iter_state = 0;
    }

    if (mc.tx_current_message) {
        REG_SIOMLT_SEND = mc.tx_current_message->data_[mc.tx_iter_state++];
    } else {
        mc.null_bytes_written += 2;
        mc.tx_iter_state++;
        REG_SIOMLT_SEND = 0;
    }
}



static void multiplayer_schedule_tx()
{
    multiplayer_tx_send();
}


static void multiplayer_serial_isr()
{
    if (multiplayer_error()) {
        return;
    }

    multiplayer_rx_receive();
    multiplayer_schedule_tx();
}


u16 mb_exchange(u16 value)
{
    REG_SIOMLT_SEND = value;
    REG_SIOCNT = REG_SIOCNT | SIO_START;
    while (REG_SIOCNT & SIO_START) ;
    u16 result = REG_SIOMULTI1;
    return result;
}


u16* isr_vram_out_addr;
volatile int isr_vram_write_state = 0;
int isr_vram_write_counter;


void multi_vram_setup_isr()
{
    switch (isr_vram_write_state) {
    case 0:
        if (REG_SIOMULTI0 == 0xABCD) {
            isr_vram_out_addr = (u16*)&MEM_TILE[4][1];
            isr_vram_write_state = 1;
            isr_vram_write_counter = 0;
        }
        break;

    case 1:
        *(isr_vram_out_addr++) = REG_SIOMULTI0;
        if (++isr_vram_write_counter == (VRAM_TILE_SIZE * 8 * 126) / sizeof(u16)) {
            // Transferred entire spritesheet...
            isr_vram_write_state = 2;
            isr_vram_write_counter = 0;
            isr_vram_out_addr = (u16*)&MEM_SCREENBLOCKS[sbb_background_texture][0];
        }
        break;

    case 2:
        *(isr_vram_out_addr++) = REG_SIOMULTI0;
        if (++isr_vram_write_counter == (VRAM_TILE_SIZE * 127) / sizeof(u16)) {
            isr_vram_write_state = 3;
            isr_vram_write_counter = 0;
            isr_vram_out_addr = (u16*)&MEM_SCREENBLOCKS[sbb_t0_texture][0];
        }
        break;

    case 3:
        *(isr_vram_out_addr++) = REG_SIOMULTI0;
        if (++isr_vram_write_counter == (VRAM_TILE_SIZE * 4 * 111) / sizeof(u16)) {
            isr_vram_write_state = 4;
            isr_vram_write_counter = 0;
            isr_vram_out_addr = (u16*)&MEM_SCREENBLOCKS[sbb_t1_texture][0];
        }
        break;

    case 4:
        *(isr_vram_out_addr++) = REG_SIOMULTI0;
        if (++isr_vram_write_counter == (VRAM_TILE_SIZE * 4 * 111) / sizeof(u16)) {
            isr_vram_write_state = 5;
            isr_vram_write_counter = 0;
        }
        break;

    case 5:
        break;
    }

    REG_SIOMLT_SEND = 0;
}



// Receive video memory from the host game
void mb_client_receive_vram()
{
    REG_RCNT = R_MULTI;
    REG_SIOCNT = SIO_MULTI;
    REG_SIOCNT = REG_SIOCNT | SIO_IRQ | SIO_115200;
    REG_SIOMLT_SEND = 0xAAAA;

    irqEnable(IRQ_SERIAL);
    irqSet(IRQ_SERIAL, multi_vram_setup_isr);

    while (isr_vram_write_state != 5) ;
}



bool multiplayer_send_message(u8* data)
{
    if (mc.tx_ring[mc.tx_ring_write_pos]) {
        WireMessage* lost_message = mc.tx_ring[mc.tx_ring_write_pos];
        mc.tx_ring[mc.tx_ring_write_pos] = NULL;
        packet_pool_free(lost_message);
    }

    WireMessage* msg = packet_pool_alloc();
    if (msg == NULL) {
        // error! Could not transmit messages fast enough, i.e. we've exhausted
        // the message pool! How to handle this condition!?
        return false;
    }

    __builtin_memcpy(msg->data_, data, 6);

    mc.tx_ring[mc.tx_ring_write_pos] = msg;
    mc.tx_ring_write_pos += 1;
    mc.tx_ring_write_pos %= TX_RING_SIZE;

    return true;
}



WireMessage* multiplayer_poll_message()
{
    if (mc.rx_iter_state == MESSAGE_ITERS) {
        return NULL;
    }
    WireMessage* msg = rx_ring_pop();
    if (msg) {
        if (mc.poller_current_message != NULL) {
            // failure to deallocate/consume message!
            // TODO: raise error...
            while (1);
        }
        mc.poller_current_message = msg;
        return msg;
    }
    return NULL;
}



void multiplayer_poll_consume()
{
    if (mc.poller_current_message) {
        packet_pool_free(mc.poller_current_message);
    } else {
        while (1) ;
    }
    mc.poller_current_message = NULL;
}



void comms_init()
{
    REG_SIOCNT = 0;

    packet_pool_init();

    for (int i = 0; i < sizeof mc; ++i) {
        ((u8*)&mc)[i] = 0;
    }

    mc.rx_current_all_zeroes = true;

    REG_RCNT = R_MULTI;
    REG_SIOCNT = SIO_MULTI;
    REG_SIOCNT = REG_SIOCNT | SIO_IRQ | SIO_115200;

    irqEnable(IRQ_SERIAL);
    irqSet(IRQ_SERIAL, multiplayer_serial_isr);

    REG_SIOMLT_SEND = 0x5555;

    while (!multiplayer_validate()) {
        // ...
    }

    const char* handshake =
        "lnsk06"; // Link cable program, skyland, 6 byte packet.

    multiplayer_send_message((u8*)handshake);

    multiplayer_schedule_tx();

    while (1) {
        WireMessage* msg = multiplayer_poll_message();
        if (msg) {
            for (u32 i = 0; i < sizeof handshake; ++i) {
                if (((u8*)msg->data_)[i] != handshake[i]) {
                    while (1) ; // failed handshake
                }
            }
            multiplayer_poll_consume(6);
            return;
        }
    }
}



static const u16 spritesheet_palette[16] = {
    0x72CC,0x2C7D,0x30CB,0x77DE,0x029E,0x45BE,0x3D34,0x6893,
    0x498C,0x7FF2,0x7DA0,0x6690,0x30C2,0x43B7,0x4244,0x1DAD
};



static const u16 island_interior_palette[16] = {
    0x72CC,0x30C2,0x498C,0x62D3,0x3E31,0x113C,0x24AD,0x35E4,
    0x43B7,0x77DE,0x4B1B,0x7BEC,0x6562,0x539B,0x3ED5,0x1DAD
};



static const u16 background_palette[16] = {
    0x72CC,0x72CC,0x77DE,0x7774,0x76AB,0x0000,0x0000,0x0000,
    0x0421,0x0421,0x0421,0x0421,0x0421,0x0421,0x0421,0x0421
};



void init_palettes()
{
    for (int i = 0; i < 16; ++i) {
        MEM_PALETTE[i] = spritesheet_palette[i];
        MEM_BG_PALETTE[i] = island_interior_palette[i];
        MEM_BG_PALETTE[i + 16] = island_interior_palette[i];
        MEM_BG_PALETTE[16 * 11 + i] = background_palette[i];
    }
}



// NOTE: see skyland/network.hpp for message definitions.
enum ServerMessageType {
#include "skyland/network_header_enum"
};



void process_server_message(u8* message)
{
    switch (message[0]) {
    case invalid:
        break;

    case heartbeat:
        send_message(message); // echo
        break;

    case room_constructed:
        // TODO...
        break;

    case terrain_constructed:
        break;

    case room_salvaged:
        break;

    default:
        // ignored...
        break;
    }
}



void multiplayer_message_loop()
{
    WireMessage* msg;
    do {
        msg = multiplayer_poll_message();
        if (msg) {
            process_server_message((u8*)msg->data_);
            multiplayer_poll_consume(6);
            return;
        }
    } while (msg);
}



int main()
{
    init_palettes();


    REG_DISPCNT = MODE_0 | OBJ_ENABLE | OBJ_MAP_1D | BG0_ENABLE | BG1_ENABLE |
                  BG2_ENABLE | BG3_ENABLE | WIN0_ENABLE;

    irqInit();

    mb_client_receive_vram();

    irqEnable(IRQ_VBLANK);

    BG0_CONTROL = BG_CBB(cbb_t0_texture) | BG_SBB(sbb_t0_tiles) | BG_REG_64x32 |
                  BG_PRIORITY(2) | BG_MOSAIC;

    BG3_CONTROL = BG_CBB(cbb_t1_texture) | BG_SBB(sbb_t1_tiles) | BG_REG_64x32 |
                  BG_PRIORITY(2) | BG_MOSAIC;

    BG1_CONTROL = BG_CBB(cbb_background_texture) | BG_SBB(sbb_bg_tiles) |
                  BG_PRIORITY(3) | BG_MOSAIC;

    BG2_CONTROL = BG_CBB(cbb_overlay_texture) | BG_SBB(sbb_overlay_tiles) |
                  BG_PRIORITY(0) | BG_MOSAIC;

    comms_init();


    while (1) {
        multiplayer_message_loop();
        VBlankIntrWait();
    }
}
