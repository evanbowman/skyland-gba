//
// An implementation of client-side rendering for a multiboot program. Mainly,
// calculations happen in the host gba, and this program just receives network
// packets and renders stuff. The entire program needs to fit in GBA RAM, so
// this code is intentionally minimal.
//


#include "../../gba.h"
#include "../../multiboot_room_metatable.h"
#include "charblock.h"
#include <stdbool.h>
#include <stddef.h>


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



bool game_paused = false;


////////////////////////////////////////////////////////////////////////////////
//
// Buttons
//
////////////////////////////////////////////////////////////////////////////////


typedef enum Key {
    key_action_1,
    key_action_2,
    key_start,
    key_select,
    key_left,
    key_right,
    key_up,
    key_down,
    key_alt_1,
    key_alt_2,
    key_count,
} Key;



bool previous_keystates[key_count];
bool current_keystates[key_count];



void Keys_init()
{
    memset(previous_keystates, 0, sizeof previous_keystates);
    memset(current_keystates, 0, sizeof current_keystates);
}



void Keys_poll()
{
    for (int i = 0; i < key_count; ++i) {
        previous_keystates[i] = current_keystates[i];
    }

#define KEYS ((volatile u32*)0x04000130)
    current_keystates[key_action_1] = ~(*KEYS) & KEY_A;
    current_keystates[key_action_2] = ~(*KEYS) & KEY_B;
    current_keystates[key_start] = ~(*KEYS) & KEY_START;
    current_keystates[key_select] = ~(*KEYS) & KEY_SELECT;
    current_keystates[key_right] = ~(*KEYS) & KEY_RIGHT;
    current_keystates[key_left] = ~(*KEYS) & KEY_LEFT;
    current_keystates[key_down] = ~(*KEYS) & KEY_DOWN;
    current_keystates[key_up] = ~(*KEYS) & KEY_UP;
    current_keystates[key_alt_1] = ~(*KEYS) & KEY_L;
    current_keystates[key_alt_2] = ~(*KEYS) & KEY_R;
}



bool is_key_down(Key k)
{
    return current_keystates[k] && !previous_keystates[k];
}



bool is_key_pressed(Key k)
{
    return current_keystates[k];
}



////////////////////////////////////////////////////////////////////////////////
//
// CursorData
//
////////////////////////////////////////////////////////////////////////////////



typedef struct CursorData
{
    s8 near_cursor_x;
    s8 near_cursor_y;
    s8 far_cursor_x;
    s8 far_cursor_y;
    u8 cursor_sprite;
    bool cursor_is_far;
    u8 cursor_anim_counter;

    s8 co_op_cursor_x;
    s8 co_op_cursor_y;
    u8 co_op_cursor_sprite;
    bool co_op_cursor_is_far;
} CursorData;



CursorData cursor_data;



static void CursorData_update()
{
    if (++cursor_data.cursor_anim_counter == 12) {
        cursor_data.cursor_anim_counter = 0;
        cursor_data.cursor_sprite = !cursor_data.cursor_sprite;
    }
}



static void CursorData_init()
{
    memset(&cursor_data, 0, sizeof cursor_data);
    cursor_data.near_cursor_x = 0;
    cursor_data.near_cursor_y = 14;
}



////////////////////////////////////////////////////////////////////////////////
//
// Camera
//
////////////////////////////////////////////////////////////////////////////////



struct Camera
{
    s16 view_offset_x;
    s16 view_offset_y;
    s16 target_y;
    s16 target_x;
} camera;



static void Camera_init()
{
    memset(&camera, 0, sizeof camera);
}



int clamp(int x, int floor, int ceil)
{
    if (x < floor) {
        return floor;
    } else if (x > ceil) {
        return ceil;
    } else {
        return x;
    }
}



////////////////////////////////////////////////////////////////////////////////
//
// Island
//
////////////////////////////////////////////////////////////////////////////////



typedef struct Island
{
    s16 x_pos;
    s16 y_pos;
    u8 terrain_size;
} Island;



Island player_island;
Island opponent_island;



static void Island_move(Island* self, u16 x_pos, u16 y_pos)
{
    self->x_pos = x_pos;
    self->y_pos = y_pos;
}



static void Island_set_positions()
{
    Island_move(&player_island, 10, 374);
    Island_move(&opponent_island, 350, 374);
}



static void Island_update(Island* island)
{
    // TODO...
}



static void Island_display(Island* island)
{
    s16 x_scroll = -island->x_pos + camera.view_offset_x;
    s16 y_scroll = -island->y_pos + camera.view_offset_y;

    if (island == &player_island) {
        BG0_X_SCROLL = x_scroll;
        BG0_Y_SCROLL = y_scroll;
    } else {
        BG3_X_SCROLL = x_scroll;
        BG3_Y_SCROLL = y_scroll;
    }
}




////////////////////////////////////////////////////////////////////////////////
//
// Scene
//
////////////////////////////////////////////////////////////////////////////////



typedef struct Scene
{
    struct Scene* (*update)(struct Scene* self);
    void (*display)();
    void (*enter)(struct Scene* prev);
    void (*exit)(struct Scene* next);
} Scene;



Scene* scene_current;



static void scene_bind(Scene* new_scene)
{
    scene_current = new_scene;
}



static void scene_update()
{
    scene_current->update(scene_current);
}



static void scene_display()
{
    scene_current->display();
}



struct ReadyScene
{
    Scene scene;
} ready_scene;



int dummy_2 = 0;


static Scene* ReadyScene_update()
{
    Island_update(&player_island);
    Island_update(&opponent_island);

    CursorData_update();

    if (is_key_down(key_right)) {
        ++cursor_data.near_cursor_x;
    }
    if (is_key_down(key_left) && cursor_data.near_cursor_x > 0) {
        --cursor_data.near_cursor_x;
    }
    if (is_key_down(key_down) && cursor_data.near_cursor_y < 15) {
        ++cursor_data.near_cursor_y;
    }
    if (is_key_down(key_up) && cursor_data.near_cursor_y > 0) {
        --cursor_data.near_cursor_y;
    }

    return NULL;
}



static void ReadyScene_display()
{
    Island_display(&player_island);
    Island_display(&opponent_island);
}



static Scene* ReadyScene_init()
{
    ready_scene.scene.update = ReadyScene_update;
    ready_scene.scene.display = ReadyScene_display;
    ready_scene.scene.enter = NULL;
    ready_scene.scene.exit = NULL;
    return &ready_scene.scene;
}



////////////////////////////////////////////////////////////////////////////////
//
// Room Metatable (received from Skyland server)
//
////////////////////////////////////////////////////////////////////////////////


SharedRoomMetatable room_metatable;



static SharedMetaclass* load_metaclass(u8 metaclass_index)
{
    for (int i = 0; i < SHARED_MT_COUNT; ++i) {
        if (room_metatable.metaclasses_[i].metaclass_index_ == metaclass_index) {
            return &room_metatable.metaclasses_[i];
        }
    }
    return NULL;
}



////////////////////////////////////////////////////////////////////////////////
//
// Tile Graphics
//
//////////////////////////////////////////////////////////////////////////////////


#define VRAM_TILE_SIZE 32


typedef enum Layer {
    layer_overlay,
    layer_background,
    layer_map_1_ext,
    layer_map_0_ext,
} Layer;



static void tile_set_16p(u8 base, u16 x, u16 y, u16 tile_id, int palette)
{
    u8 screen_block = base;
    if (x > 15) {
        x %= 16;
        screen_block = base + 1;
    }
#define REF(X, Y) ((X) * 2 + (Y) * 32 * 2)

    MEM_SCREENBLOCKS[screen_block][0 + REF(x % 16, y)] =
        (tile_id * 4 + 0) | SE_PALBANK(palette);

    MEM_SCREENBLOCKS[screen_block][1 + REF(x % 16, y)] =
        (tile_id * 4 + 1) | SE_PALBANK(palette);

    MEM_SCREENBLOCKS[screen_block][0 + REF(x % 16, y) + 32] =
        (tile_id * 4 + 2) | SE_PALBANK(palette);

    MEM_SCREENBLOCKS[screen_block][1 + REF(x % 16, y) + 32] =
        (tile_id * 4 + 3) | SE_PALBANK(palette);

#undef REF
}



static void tile_set(Layer layer,
              u16 x,
              u16 y,
              u16 val)
{
    switch (layer) {

    default:
        break;

    /* case Layer::overlay: */
    /*     if (x > 31 or y > 31) { */
    /*         return; */
    /*     } */
    /*     set_overlay_tile(*this, x, y, val, 1); */
    /*     break; */

    case layer_map_1_ext:
        tile_set_16p(sbb_t1_tiles, x, y, val, 1);
        break;

    case layer_map_0_ext:
        tile_set_16p(sbb_t0_tiles, x, y, val, 0);
        break;

    case layer_background:
        if (x > 31 || y > 31) {
            return;
        }
        MEM_SCREENBLOCKS[sbb_bg_tiles][x + y * 32] = val | SE_PALBANK(11);
        break;
    }
}



static void cloud_block_put(int x, int y, int offset)
{
    tile_set(layer_background, x, y, offset++);
    tile_set(layer_background, x + 1, y, offset++);
    tile_set(layer_background, x, y + 1, offset++);
    tile_set(layer_background, x + 1, y + 1, offset);
}



static void cloud_block_put_fg(int x, int type)
{
    cloud_block_put(x * 2, 16, 8 + type * 4);
    cloud_block_put(x * 2, 18, 48 + type * 4);
}



static void cloud_block_put_bg(int x, int type)
{
    cloud_block_put(x * 2, 14, 32 + type * 4);
}



static void terrain_init(Layer layer, u8 size)
{
    int palette = 0;
    if (layer == layer_map_1_ext) {
        palette = 2;
    }

    tile_set_16p(layer, 0, 15, 9, palette);
    for (int x = 0; x < size - 2; ++x) {
        tile_set_16p(layer, 1 + x, 15, 8, palette);
    }
    tile_set_16p(layer, size - 1, 15, 10, palette);
}



static void clouds_init()
{
    for (int i = 0; i < 32; ++i) {
        for (int j = 0; j < 32; ++j) {
            tile_set(layer_background, i, j, 4);
        }
    }

    for (int x = 0; x < 32; ++x) {
        for (int y = 0; y < 2; ++y) {
            tile_set(layer_background, x, y, 72);
        }
        for (int y = 2; y < 4; ++y) {
            tile_set(layer_background, x, y, 73);
        }
        for (int y = 4; y < 6; ++y) {
            tile_set(layer_background, x, y, 74);
        }
        for (int y = 6; y < 8; ++y) {
            tile_set(layer_background, x, y, 75);
        }
    }

    for (int i = 0; i < 32; ++i) {
        tile_set(layer_background, i, 18, 5);
        tile_set(layer_background, i, 19, 5);
    }

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 6;
        cloud_block_put_fg(offset + 0, 0);
        cloud_block_put_fg(offset + 1, 1);
        cloud_block_put_fg(offset + 2, 2);
        cloud_block_put_fg(offset + 3, 3);
        cloud_block_put_fg(offset + 4, 4);
        cloud_block_put_fg(offset + 5, 5);
    }

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 4;
        cloud_block_put_bg(offset + 0, 0);
        cloud_block_put_bg(offset + 1, 1);
        cloud_block_put_bg(offset + 2, 2);
        cloud_block_put_bg(offset + 3, 3);
    }
}



////////////////////////////////////////////////////////////////////////////////
//
// Multiplayer Protocol
//
////////////////////////////////////////////////////////////////////////////////


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


static void packet_pool_init()
{
    packet_pool = NULL;

    for (int i = 0; i < PACKET_POOL_SIZE; ++i) {
        WireMessage* current = packet_pool_data + i;
        current->next_ = packet_pool;
        packet_pool = current;
    }
}



static WireMessage* packet_pool_alloc()
{
    if (packet_pool) {
        WireMessage* result = packet_pool;
        packet_pool = result->next_;
        return result;
    } else {
        return NULL;
    }
}



static void packet_pool_free(WireMessage* packet)
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


u16* isr_vram_out_addr;
volatile int isr_vram_write_state = 0;
int isr_vram_write_counter;


static void multi_vram_setup_isr()
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
            isr_vram_out_addr = (u16*)&room_metatable;
        }
        break;

    case 5:
        *(isr_vram_out_addr++) = REG_SIOMULTI0;
        if (++isr_vram_write_counter == sizeof(room_metatable) / sizeof(u16)) {
            isr_vram_write_state = 6;
            isr_vram_write_counter = 0;
        }
        break;

    case 6:
        // ...
        break;
    }

    REG_SIOMLT_SEND = 0;
}



// Receive video memory from the host game
static void mb_client_receive_vram()
{
    REG_RCNT = R_MULTI;
    REG_SIOCNT = SIO_MULTI;
    REG_SIOCNT = REG_SIOCNT | SIO_IRQ | SIO_115200;
    REG_SIOMLT_SEND = 0xAAAA;

    irqEnable(IRQ_SERIAL);
    irqSet(IRQ_SERIAL, multi_vram_setup_isr);

    while (isr_vram_write_state != 6) ;
}



static bool multiplayer_send_message(u8* data)
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



static WireMessage* multiplayer_poll_message()
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



static void multiplayer_poll_consume()
{
    if (mc.poller_current_message) {
        packet_pool_free(mc.poller_current_message);
    } else {
        while (1) ;
    }
    mc.poller_current_message = NULL;
}



static void comms_init()
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



static void init_palettes()
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


typedef u8 ClientMessage[6];



static void draw_room(u8 metaclass_index, u8 rx, u8 ry, Layer layer)
{
    SharedMetaclass* mt = load_metaclass(metaclass_index);
    if (mt) {

        int t = 0;
        for (int y = 0; y < mt->size_y_; ++y) {
            for (int x = 0; x < mt->size_x_; ++x) {
                tile_set(layer,
                         rx + x,
                         ry + y,
                         mt->tiles_[t++]);
            }
        }
    }
}



static void process_server_message(u8* message)
{
    switch (message[0]) {
    case invalid:
        break;

    case heartbeat:
        multiplayer_send_message(message); // echo
        break;

    case opponent_room_created:
        draw_room(message[1], message[3], message[4], layer_map_1_ext);
        break;

    case room_constructed:
        draw_room(message[1], message[3], message[4], layer_map_0_ext);
        break;

    case terrain_constructed:
        terrain_init(layer_map_0_ext, message[1]);
        break;

    case room_destroyed:
        // TODO...
        break;

    case room_salvaged:
        // TODO...
        break;

    case co_op_room_lock_acquire: {
        // FIXME: this code yields the lock to the host console in all
        // cases... but is that really a problem? Just let the server host be
        // the ultimate decider upon any conflicts.
        ClientMessage out;
        out[0] = co_op_room_lock_response;
        out[1] = message[1];
        out[2] = message[2];
        out[3] = 0;
        multiplayer_send_message(out);
        break;
    }

    case co_op_cursor: {
        cursor_data.co_op_cursor_x = message[1];
        cursor_data.co_op_cursor_y = message[2];
        cursor_data.co_op_cursor_is_far = !message[3];
        cursor_data.co_op_cursor_sprite = message[4];
        break;
    }

    case paused:
        game_paused = message[1];
        break;

    case co_op_sync_begin:
        // TODO...
        break;

    case co_op_sync_block:
        draw_room(message[1], message[2], message[3], layer_map_0_ext);
        break;

    case co_op_sync_end:
        // TODO...
        break;

    default:
        // ignored...
        break;
    }
}



static void Camera_update()
{
    s8 cursor_loc_x;
    s8 cursor_loc_y;

    if (cursor_data.cursor_is_far) {
        cursor_loc_x = cursor_data.far_cursor_x;
        cursor_loc_y = cursor_data.far_cursor_y;
    } else {
        cursor_loc_x = cursor_data.near_cursor_x;
        cursor_loc_y = cursor_data.near_cursor_y;
    }

    camera.target_y = (-((15 - (cursor_loc_y + 1)) * 16) / 2);
    camera.target_y = clamp(camera.target_y, -80, 0);

    s16 tpos_x = player_island.x_pos;

    int x = cursor_loc_x;
    if (x == 255) {
        x = -1;
    }

    if (!cursor_data.cursor_is_far) {
        camera.target_x = tpos_x + ((x - 3) * 16) / 2;
        camera.target_x = clamp(camera.target_x, (int)tpos_x - 40, (int)tpos_x + 48);
        camera.target_x -= 16;
    } else {
        camera.target_x = tpos_x + ((x + 3) * 16) / 2;
        camera.target_x = clamp(camera.target_x, (int)tpos_x - 48, (int)tpos_x + 256);
        camera.target_x -= 100;
    }

    camera.view_offset_x = (4 * camera.view_offset_x + 4 * camera.target_x) / 8;
    camera.view_offset_y = (4 * camera.view_offset_y + 4 * camera.target_y) / 8;
}




static void multiplayer_message_loop()
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
                  BG2_ENABLE | BG3_ENABLE /* | WIN0_ENABLE */;

    BG0_CONTROL = BG_CBB(cbb_t0_texture) | BG_SBB(sbb_t0_tiles) | BG_REG_64x32 |
                  BG_PRIORITY(2) | BG_MOSAIC;

    BG3_CONTROL = BG_CBB(cbb_t1_texture) | BG_SBB(sbb_t1_tiles) | BG_REG_64x32 |
                  BG_PRIORITY(2) | BG_MOSAIC;

    BG1_CONTROL = BG_CBB(cbb_background_texture) | BG_SBB(sbb_bg_tiles) |
                  BG_PRIORITY(3) | BG_MOSAIC;

    BG2_CONTROL = BG_CBB(cbb_overlay_texture) | BG_SBB(sbb_overlay_tiles) |
                  BG_PRIORITY(0) | BG_MOSAIC;

    irqInit();

    mb_client_receive_vram();

    irqEnable(IRQ_VBLANK);

    comms_init();

    clouds_init();

    Island_set_positions();

    CursorData_init();
    Camera_init();

    scene_bind(ReadyScene_init());

    while (1) {
        multiplayer_message_loop();

        ClientMessage cursor_msg;
        cursor_msg[0] = co_op_cursor;
        cursor_msg[1] = cursor_data.near_cursor_x;
        cursor_msg[2] = cursor_data.near_cursor_y;
        cursor_msg[3] = !cursor_data.cursor_is_far;
        cursor_msg[4] = cursor_data.cursor_sprite;
        multiplayer_send_message(cursor_msg);

        Camera_update();

        Keys_poll();
        scene_update();
        VBlankIntrWait();
        scene_display();
    }
}
