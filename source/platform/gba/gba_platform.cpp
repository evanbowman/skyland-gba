////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
//
//
// Platform Implementation for Nintendo Gameboy Advance
//
// Generally, the code in this file is a mess, as the gba hardware is set in
// stone and I don't need to maintain this stuff, so I didn't bother to write
// organized code.
//
//
////////////////////////////////////////////////////////////////////////////////

#ifdef __GBA__

#include "allocator.hpp"
#include "bootleg_cart.hpp"
#include "containers/vector.hpp"
#include "critical_section.hpp"
#include "filesystem.hpp"
#include "gbp_logo.hpp"
#include "graphics/overlay.hpp"
#include "images.cpp"
#include "localization.hpp"
#include "mixer.hpp"
#include "number/random.hpp"
#include "platform/color.hpp"
#include "platform/conf.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "rumble.h"
#include "script/lisp.hpp"
#include "send_multiboot.h"
#include "skyland_mgba.hpp"
#include "string.hpp"
#include "util.hpp"
#include <algorithm>
#include <limits>
#include <setjmp.h>


extern "C" {

int strcmp(const char* p1, const char* p2)
{
    return str_cmp(p1, p2);
}


void __cxa_pure_virtual()
{
    Platform::fatal("pure virtual call!");
}
}



u8* color_correction_lut = nullptr;



extern char __iwram_overlay_end;
extern char __text_start;
extern char __iwram_start__;
extern char __data_end__;
extern char __ewram_start;
extern char __eheap_start;
extern char __rom_end__;



// Explanation: iwram_overlay_end points to the end of the stack. I write 16
// bytes of stack canary data, and perform a stack overflow check at least once
// per frame, to determine whether the software exceeded the stack limits.
static const char* stack_canary_value = "（・θ・）";



inline void* stack_end()
{
    return &__iwram_overlay_end;
}



inline u32 stack_reserved_size()
{
    constexpr u32 iwram_size = 32768;
    return iwram_size - (&__data_end__ - &__iwram_start__);
}



inline u32 max_stack_usage()
{
    // We use the fact that the crt0 initializes iwram to zero to estimate
    // maximum stack usage (how much still-zeroed memory remains?). Obviously
    // this breaks down if variables written to the stack are subsequently
    // zeroed...

    u32 stack_rem = 0;

    auto addr = (char*)stack_end() + 16; // add stack canary size
    char dummy = 0;

    while (addr < &dummy) {
        if (*addr == 0) {
            ++stack_rem;
        } else {
            break;
        }
        ++addr;
    }

    return stack_reserved_size() - stack_rem;
}



static void canary_init()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wstringop-overflow"

    __builtin_memcpy(stack_end(), stack_canary_value, 16);

#pragma GCC diagnostic pop
}



static inline bool canary_check()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wstringop-overflow"

    return __builtin_memcmp(stack_end(), stack_canary_value, 16) == 0;

#pragma GCC diagnostic pop
}



#include "gba.h"



#define REG_DEBUG_ENABLE (volatile u16*)0x4FFF780
#define REG_DEBUG_FLAGS (volatile u16*)0x4FFF700
#define REG_DEBUG_STRING (char*)0x4FFF600
#define MGBA_LOG_INFO 3



extern "C" {


int mgba_detect()
{
    *REG_DEBUG_ENABLE = 0xC0DE;
    return *REG_DEBUG_ENABLE == 0x1DEA;
}


void mgba_log(const char* str)
{
    auto len = strlen(str);
    if (len > 0x100) {
        len = 0x100;
    }
    for (u32 i = 0; i < len; ++i) {
        (REG_DEBUG_STRING)[i] = str[i];
    }
    *REG_DEBUG_FLAGS = MGBA_LOG_INFO | 0x100;
}
}



static void __attribute__((noinline)) busy_wait(unsigned max)
{
    for (unsigned i = 0; i < max; i++) {
        __asm__ volatile("" : "+g"(i) : :);
    }
}



namespace
{

class RemoteConsoleLispPrinter : public lisp::Printer
{
public:
    RemoteConsoleLispPrinter()
    {
    }

    void put_str(const char* str) override
    {
        fmt_ += str;
    }

    Platform::RemoteConsole::Line fmt_;
};

} // namespace



static EWRAM_DATA ScreenBlock overlay_back_buffer alignas(u32);
static bool overlay_back_buffer_changed = false;



alignas(4) static EWRAM_DATA u16 sp_palette_back_buffer[32];
alignas(4) static EWRAM_DATA u16 bg_palette_back_buffer[256];



static int overlay_y = 0;



enum class GlobalFlag {
    rtc_faulty,
    gbp_unlocked,
    multiplayer_connected,
    save_using_flash,
    glyph_mode,
    parallax_clouds,
    v_parallax,
    partial_palette_sync,
    palette_sync,
    sound_startup_monkeypatch,
    key_poll_called,
    watchdog_disabled,
    effect_window_mode,
    iris_effect_mode,
    skyland_custom_mgba,
    count
};


static Bitvector<static_cast<int>(GlobalFlag::count)> gflags;



static enum class ModelId : u8 { gba, nds, gbmicro, gbplayer } model_id;



Platform::ModelName Platform::model_name() const
{
    switch (model_id) {
    default:
    case ModelId::gba:
        return "AGB_OR_AGS";
    case ModelId::nds:
        return "NDS";
    case ModelId::gbmicro:
        return "MICRO";
    case ModelId::gbplayer:
        return "GBP";
    }
}



static void set_gflag(GlobalFlag f, bool val)
{
    gflags.set(static_cast<int>(f), val);
}


static bool get_gflag(GlobalFlag f)
{
    return gflags.get(static_cast<int>(f));
}


struct BiosVersion
{
    enum {
        NDS = static_cast<long unsigned int>(-1162995584),
        GBA = static_cast<long unsigned int>(-1162995585)
    };
};



Platform::DeviceName Platform::device_name() const
{
    return "GameboyAdvance";
}



// These word and halfword versions of memcpy are written in assembly. They use
// special ARM instructions to copy data faster than you could do with thumb
// code.
extern "C" {
__attribute__((section(".iwram"), long_call)) void
memcpy32(void* dst, const void* src, uint wcount);
void memcpy16(void* dst, const void* src, uint hwcount);

__attribute__((section(".iwram"), long_call)) void
memset32(void* dst, u32 src, u32 wdn);
void memset16(void* dst, u16 src, u32 hwn);
}



void Platform::memset_words(void* dest, u8 byte, u32 word_count)
{
    memset32(dest, byte | byte << 8 | byte << 16 | byte << 24, word_count);
}



// Used for software rendering, needs to be stored in iwram and heavily
// optimized.
__attribute__((section(".iwram"), long_call)) void blit_tile(u16* out, u16* in);

__attribute__((section(".iwram"), long_call)) void
win_circle(u16 winh[], int x0, int y0, int rr);


__attribute__((section(".iwram"), long_call)) void audio_update_fast_isr();


static int audio_timer_frequency(int words_per_call)
{
    // NOTE: 0xffff represents the max timer value. The subtracted value
    // represents the number of timer tics to wait before running the irq
    // again. Upon each tic of the timer, the direct sound chip loads one sample
    // from audio FIFO A. The fifo is a 32bit register, so we write four samples
    // to the fifo at a time.
    return 0xffff - ((4 * words_per_call) - 1);
}



////////////////////////////////////////////////////////////////////////////////
//
// Tile Memory Layout:
//
// The game uses every single available screen block, so the data is fairly
// tightly packed. Here's a chart representing the layout:
//
// All units of length are in screen blocks, followed by the screen block
// indices in parentheses.
//
//     charblock 0        charblock 1      charblock 2
// ~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~
// o======================================================
// |    t0 texture    |   t1 texture   | overlay texture |
// |   len 8 (0 - 7)  | len 8 (8 - 15) | len 8 (16 - 23) | ...
// o======================================================
//
//                  charblock 3
//      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//     ====================================o
//     |     t0 mem      |     t1 mem      |
// ... | len 2 (26 - 27) | len 2 (28 - 29) | ...
//     ====================================o
//
//                        charblock 3 (contd.)
//      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//     ========================================================o
//     |   background texture    |  overlay mem  |   bg mem    |
// ... |    len 1 (24 - 25)      |  len 1 (30)   | len 1 (31)  |
//     ========================================================o
//
//


static constexpr const int sbb_per_cbb = 8; // ScreenBaseBlock per CharBaseBlock

static constexpr const int charblock_size = sizeof(ScreenBlock) * sbb_per_cbb;

static constexpr const int sbb_t0_tiles = 26;
static constexpr const int sbb_t1_tiles = 28;
static constexpr const int sbb_overlay_tiles = 30;
static constexpr const int sbb_bg_tiles = 31;

static constexpr const int sbb_background_texture = 24;
static constexpr const int cbb_background_texture =
    sbb_background_texture / sbb_per_cbb;

static constexpr const int sbb_overlay_texture = 16;
static constexpr const int sbb_t0_texture = 0;
static constexpr const int sbb_t1_texture = 8;

static constexpr const int cbb_overlay_texture =
    sbb_overlay_texture / sbb_per_cbb;

static constexpr const int cbb_t0_texture = sbb_t0_texture / sbb_per_cbb;
static constexpr const int cbb_t1_texture = sbb_t1_texture / sbb_per_cbb;


//
//
////////////////////////////////////////////////////////////////////////////////


static constexpr int vram_tile_size()
{
    // 8 x 8 x (4 bitsperpixel / 8 bitsperbyte)
    return 32;
}



static const TextureData* current_spritesheet = &sprite_textures[0];
static const TextureData* current_tilesheet0 = &tile_textures[0];
static const TextureData* current_tilesheet1 = &tile_textures[1];
static const TextureData* current_overlay_texture = &overlay_textures[1];
static const TextureData* current_background = &tile_textures[0];


void start(Platform&);


Platform* __platform__;



Platform& Platform::instance()
{
    return *__platform__;
}



// NOTE: If we encountered a stack overflow, me may not have enough stack space
// to print out an error message without messing things up even more. So, save a
// context from which we can safely display a stack overflow message.
static EWRAM_DATA jmp_buf stack_overflow_resume_context;



static inline void on_stack_overflow();



EWRAM_DATA
static Optional<Platform::UnrecoverrableErrorCallback>
    unrecoverrable_error_callback;



int main(int argc, char** argv)
{
    canary_init();

    gflags.clear();

    Platform pf;

    const int resume = setjmp(stack_overflow_resume_context);
    if (resume) {
        ::__platform__ = &pf; // In case it was corrupted.
        // NOTE: assigning the global platform pointer from a cached version on
        // the stack is fine. We overflowed the stack into iwram, but the stuff
        // on the stack itself is not corrupted.

        if (unrecoverrable_error_callback) {
            (*unrecoverrable_error_callback)("stack overflow");
        }
        on_stack_overflow();
    }

    start(pf);

    return 0;
}



void print_char(utf8::Codepoint c,
                const OverlayCoord& coord,
                const Optional<FontColors>& colors = {});



static inline void on_stack_overflow()
{
    static constexpr const auto bkg_color = custom_color(0xcb1500);
    ::__platform__->screen().fade(1.f, bkg_color);
    ::__platform__->fill_overlay(0);
    irqDisable(IRQ_TIMER2 | IRQ_TIMER3 | IRQ_VBLANK);

    ::__platform__->speaker().stop_music();

    ::__platform__->enable_glyph_mode(false);
    __platform__->fill_overlay(0);

    ::__platform__->load_overlay_texture("stack_overflow_flattened");

    draw_image(1, 0, 3, 30, 12, Layer::overlay);

    memcpy32(MEM_SCREENBLOCKS[sbb_overlay_tiles],
             overlay_back_buffer,
             (sizeof(u16) * (21 * 32)) / 4);

    __platform__->screen().clear();
    __platform__->screen().display();

    while (true)
        ;
}



////////////////////////////////////////////////////////////////////////////////
// DeltaClock
////////////////////////////////////////////////////////////////////////////////


Platform::DeltaClock::DeltaClock() : impl_(nullptr)
{
}


static size_t delta_total;


static int delta_read_tics()
{
    return REG_TM3CNT_L + delta_total;
}


static Microseconds delta_convert_tics(int tics)
{
    //
    // IMPORTANT: Already well into development, I discovered that the Gameboy
    // Advance does not refresh at exactly 60 frames per second. Rather than
    // change all of the code, I am going to keep the timestep as-is. Anyone
    // porting the code to a new platform should make the appropriate
    // adjustments in their implementation of DeltaClock. I believe the actual
    // refresh rate on the GBA is something like 59.59.
    //
    // P.S.: Now, I've discovered that the screen refresh rate is actually 59.73
    // Hz. Sorry to have created a headache for anyone in the future who may be
    // attempting to port this game.
    //
    return (tics * 59.59f) / 1000.f;
}


Platform::DeltaClock::TimePoint Platform::DeltaClock::sample() const
{
    return delta_read_tics();
}


Microseconds Platform::DeltaClock::duration(TimePoint t1, TimePoint t2)
{
    return delta_convert_tics(t2 - t1);
}



static EWRAM_DATA Microseconds last_delta = 0;



Microseconds Platform::DeltaClock::reset()
{
    // (1 second / 60 frames) x (1,000,000 microseconds / 1 second) =
    // 16,666.6...

    irqDisable(IRQ_TIMER3);
    const auto tics = delta_read_tics();
    REG_TM3CNT_H = 0;

    irqEnable(IRQ_TIMER3);

    delta_total = 0;

    REG_TM3CNT_L = 0;
    REG_TM3CNT_H = 1 << 7 | 1 << 6;

    ::last_delta = delta_convert_tics(tics);
    return ::last_delta;
}



Microseconds Platform::DeltaClock::last_delta() const
{
    return ::last_delta;
}



Platform::DeltaClock::~DeltaClock()
{
}


////////////////////////////////////////////////////////////////////////////////
// Keyboard
////////////////////////////////////////////////////////////////////////////////



Optional<Bitvector<int(Key::count)>> missed_keys;



static void poll_keys(Platform::Keyboard::KeyStates& k)
{
    volatile u32* keys = (volatile u32*)0x04000130;

    k[int(Key::action_1)] = ~(*keys) & KEY_A;
    k[int(Key::action_2)] = ~(*keys) & KEY_B;
    k[int(Key::start)] = ~(*keys) & KEY_START;
    k[int(Key::select)] = ~(*keys) & KEY_SELECT;
    k[int(Key::right)] = ~(*keys) & KEY_RIGHT;
    k[int(Key::left)] = ~(*keys) & KEY_LEFT;
    k[int(Key::down)] = ~(*keys) & KEY_DOWN;
    k[int(Key::up)] = ~(*keys) & KEY_UP;
    k[int(Key::alt_1)] = ~(*keys) & KEY_L;
    k[int(Key::alt_2)] = ~(*keys) & KEY_R;
}



void Platform::Keyboard::poll()
{
    set_gflag(GlobalFlag::key_poll_called, true);

    std::copy(std::begin(states_), std::end(states_), std::begin(prev_));

    poll_keys(states_);

    if (UNLIKELY(static_cast<bool>(::missed_keys))) {
        for (int i = 0; i < (int)Key::count; ++i) {
            if ((*::missed_keys)[i]) {
                states_[i] = true;
            }
        }
        ::missed_keys.reset();
    }
}


void Platform::Keyboard::rumble(bool enabled)
{
    // We have a working RTC chip connected to the cartridge gpio, so do not
    // attempt to write rumble commands.
    if (not get_gflag(GlobalFlag::rtc_faulty)) {
        return;
    }

    if (enabled) {
        rumble_set_state(RumbleState::rumble_start);
    } else {
        rumble_set_state(RumbleState::rumble_stop);
    }
}


////////////////////////////////////////////////////////////////////////////////
// Screen
////////////////////////////////////////////////////////////////////////////////


struct alignas(4) ObjectAttributes
{
    u16 attribute_0;
    u16 attribute_1;
    u16 attribute_2;

    s16 affine_transform;
};


// See documentation. Object memory provides thirty-two matrices for affine
// transformation; the parameters nestled between every four objects.
struct alignas(4) ObjectAffineMatrix
{
    ObjectAttributes o0;
    ObjectAttributes o1;
    ObjectAttributes o2;
    ObjectAttributes o3;

    auto& pa()
    {
        return o0.affine_transform;
    }
    auto& pb()
    {
        return o1.affine_transform;
    }
    auto& pc()
    {
        return o2.affine_transform;
    }
    auto& pd()
    {
        return o3.affine_transform;
    }

    void identity()
    {
        pa() = 0x0100l;
        pb() = 0;
        pc() = 0;
        pd() = 0x0100;
    }

    void scale(s16 sx, s16 sy)
    {
        pa() = (1 << 8) - sx;
        pb() = 0;
        pc() = 0;
        pd() = (1 << 8) - sy;
    }

    void rotate(s16 degrees)
    {
        // I have no recollection of why the shift by seven works. I saw some
        // libraries shift by four, but that seemed not to work from what I
        // remember. Everyone seems to use a different sine lookup table, might
        // be the culprit.
        const int ss = sine(degrees) >> 7;
        const int cc = cosine(degrees) >> 7;

        pa() = cc;
        pb() = -ss;
        pc() = ss;
        pd() = cc;
    }

    void rot_scale(s16 degrees, s16 x, s16 y)
    {
        // FIXME: This code doesn't seem to work correctly yet...
        const int ss = sine(degrees);
        const int cc = cosine(degrees);

        pa() = cc * x >> 12;
        pb() = -ss * x >> 12;
        pc() = ss * y >> 12;
        pd() = cc * y >> 12;
    }
};


namespace attr0_mask
{
constexpr u16 disabled{2 << 8};
}


constexpr u32 oam_count = Platform::Screen::sprite_limit;


static ObjectAttributes* const object_attribute_memory = {
    (ObjectAttributes*)0x07000000};

static ObjectAttributes
    object_attribute_back_buffer[Platform::Screen::sprite_limit];


static auto affine_transform_back_buffer =
    reinterpret_cast<ObjectAffineMatrix*>(object_attribute_back_buffer);


static const u32 affine_transform_limit = 32;
static u32 affine_transform_write_index = 0;
static u32 last_affine_transform_write_index = 0;



static u8 last_fade_amt;
static ColorConstant last_color;
static bool last_fade_include_sprites;


#define REG_BLENDCNT (*((volatile u16*)0x04000050))
#define REG_BLENDALPHA (*((volatile u16*)0x04000052))

#define BLD_BUILD(top, bot, mode)                                              \
    ((((bot) & 63) << 8) | (((mode) & 3) << 6) | ((top) & 63))
#define BLD_OBJ 0x0010
#define BLD_BG0 0x0001
#define BLD_BG1 0x0002
#define BLD_BG3 0x0008
#define BLDA_BUILD(eva, evb) (((eva) & 31) | (((evb) & 31) << 8))


static auto blend(const Color& c1, const Color& c2, u8 amt)
{
    switch (amt) {
    case 0:
        return c1.bgr_hex_555();
    case 255:
        return c2.bgr_hex_555();
    default:
        return Color(fast_interpolate(c2.r_, c1.r_, amt),
                     fast_interpolate(c2.g_, c1.g_, amt),
                     fast_interpolate(c2.b_, c1.b_, amt))
            .bgr_hex_555();
    }
}



void window_init_default()
{
    set_gflag(GlobalFlag::effect_window_mode, false);
    set_gflag(GlobalFlag::iris_effect_mode, false);

    REG_WININ = WIN_ALL | WIN_BLD;
    // Outside the window, display the background and the overlay, also objects.
    REG_WINOUT = WIN_OBJ | WIN_BG1 | WIN_BG2;
}



void window_init_effectmode()
{
    set_gflag(GlobalFlag::effect_window_mode, true);

    REG_WININ = WIN_ALL;
    // everything but the overlay:
    REG_WINOUT = WIN_BG1 | WIN_BG3 | WIN_BG0 | WIN_OBJ;

    REG_WIN0V = 160;
    REG_WIN0H = 240;
}



void window_init_inverse_effectmode()
{
    set_gflag(GlobalFlag::effect_window_mode, true);
    set_gflag(GlobalFlag::iris_effect_mode, true);

    REG_WINOUT = WIN_ALL;
    REG_WININ = WIN_BG1 | WIN_BG3 | WIN_BG0 | WIN_OBJ;

    REG_WIN0V = 160;
    REG_WIN0H = 240;
}



void Platform::override_priority(Layer layer, int priority)
{
    switch (layer) {
    default:
        // TODO...
        break;

    case Layer::map_0:
        BG0_CONTROL = BG0_CONTROL & ~0x3;
        BG0_CONTROL = BG0_CONTROL | BG_PRIORITY(priority);
        break;

    case Layer::map_1:
        BG3_CONTROL = BG3_CONTROL & ~0x3;
        BG3_CONTROL = BG3_CONTROL | BG_PRIORITY(priority);
        break;
    }
}



// Most of the game uses tile-based graphics modes, but some parts of the intro
// sequence, which display the gameboy player logo, currently use the bitmap
// graphics modes, for simplicity.
static void init_video(Platform::Screen& screen)
{
    REG_DISPCNT = MODE_0 | OBJ_ENABLE | OBJ_MAP_1D | BG0_ENABLE | BG1_ENABLE |
                  BG2_ENABLE | BG3_ENABLE | WIN0_ENABLE;

    window_init_default();

    REG_BLENDCNT = BLD_BUILD(0, BLD_BG0 | BLD_BG1 | BLD_BG3, 0);

    REG_BLENDALPHA = BLDA_BUILD(0x40 / 8, 0x40 / 8);

    // Tilemap layer 0
    BG0_CONTROL = BG_CBB(cbb_t0_texture) | BG_SBB(sbb_t0_tiles) | BG_REG_64x32 |
                  BG_PRIORITY(2) | BG_MOSAIC;

    // Tilemap layer 1
    BG3_CONTROL = BG_CBB(cbb_t1_texture) | BG_SBB(sbb_t1_tiles) | BG_REG_64x32 |
                  BG_PRIORITY(2) | BG_MOSAIC;

    // The background
    BG1_CONTROL = BG_CBB(cbb_background_texture) | BG_SBB(sbb_bg_tiles) |
                  BG_PRIORITY(3) | BG_MOSAIC;

    // The overlay
    BG2_CONTROL = BG_CBB(cbb_overlay_texture) | BG_SBB(sbb_overlay_tiles) |
                  BG_PRIORITY(0) | BG_MOSAIC;

    View view;
    view.set_size(screen.size().cast<Float>());

    screen.set_view(view);

    REG_MOSAIC = MOS_BUILD(0, 0, 1, 1);
}


static bool unlock_gameboy_player()
{
    volatile u32* keys = (volatile u32*)0x04000130;

    bool gbp_detected = false;

    RegisterRamReset(RESET_VRAM);


    REG_DISPCNT = MODE_0 | BG0_ENABLE;
    BG0_CONTROL = 0x0088;
    BG0_X_SCROLL = 0;
    BG0_Y_SCROLL = 0;

    static constexpr const auto white_555 =
        Color(custom_color(0xffffff)).bgr_hex_555();

    // The fade effect is costly, pre-populate the palette with the start color
    // to mitigate (visible) tearing.
    for (u32 i = 0; i < sizeof(gbp_logo_palette) / 2; ++i) {
        MEM_BG_PALETTE[i] = white_555;
    }

    auto push_palette = [&](const Color& to, u8 blend_amount) {
        if (blend_amount == 0) {
            memcpy16(MEM_BG_PALETTE,
                     gbp_logo_palette,
                     (sizeof gbp_logo_palette) / 2);
            return;
        }
        static const int palette_count = (sizeof gbp_logo_palette) / 2;
        for (int i = 0; i < palette_count; ++i) {
            const auto c = Color::from_bgr_hex_555(((u16*)gbp_logo_palette)[i]);
            MEM_BG_PALETTE[i] = blend(c, to, blend_amount);
        }
    };

    // Show the Gameboy Player splash screen.
    memcpy16((u16*)0x6008000, gbp_logo_pixels, (sizeof gbp_logo_pixels) / 2);
    memcpy16((u16*)0x6000000, gbp_logo_tiles, (sizeof gbp_logo_tiles) / 2);

    static const int fadein_frames = 15;
    static const int fadeout_frames = 10;

    // The fadein/out effects are not required for gbp unlocking. Just looks
    // clean.
    for (int i = 0; i < fadein_frames; ++i) {
        push_palette(custom_color(0xffffff),
                     (1 - Float(i) / fadein_frames) * 255);
        // The gameboy player will probably not be unlocked during the fade, but
        // you never know, I guess...
        if (*keys == 0x030F) {
            gbp_detected = true;
        }

        VBlankIntrWait();
    }

    push_palette(ColorConstant::rich_black, 0);

    using Frames = int;
    static const Frames splashscreen_duration(25);

    for (Frames i = 0; i < splashscreen_duration; ++i) {
        // If the gameboy player hardware/software accepted our spash screen,
        // the system will raise a joystick state where L+R+U+D on the D-Pad are
        // all pressed at once. If we see this keymask, we know that we have
        // successfully unlocked the gameboy player.
        if (*keys == 0x030F) {
            gbp_detected = true;
        } else if (~(*keys) & KEY_B) {
            break;
        }

        VBlankIntrWait();
    }

    for (int i = 0; i < fadeout_frames; ++i) {
        push_palette(custom_color(0xffffff), (Float(i) / fadeout_frames) * 255);
        VBlankIntrWait();
    }

    push_palette(custom_color(0xffffff), 255);

    for (int i = 0; i < 10; ++i) {
        VBlankIntrWait();
    }

    RegisterRamReset(RESET_VRAM);

    return gbp_detected;
}


Platform::Screen::Screen() : userdata_(nullptr)
{
}


static u32 last_oam_write_index = 0;
static u32 oam_write_index = 0;


Color real_color(ColorConstant k)
{
    switch (k) {
    case ColorConstant::electric_blue:
        static constexpr const Color el_blue(0, 31, 31);
        return el_blue;

    case ColorConstant::turquoise_blue:
        static constexpr const Color turquoise_blue(0, 31, 27);
        return turquoise_blue;

    case ColorConstant::cerulean_blue:
        static constexpr const Color cerulean_blue(12, 27, 31);
        return cerulean_blue;

    case ColorConstant::picton_blue:
        static constexpr const Color picton_blue(9, 20, 31);
        return picton_blue;

    case ColorConstant::maya_blue:
        static constexpr const Color maya_blue(10, 23, 31);
        return maya_blue;

    case ColorConstant::aged_paper:
        static constexpr const Color aged_paper(27, 24, 18);
        return aged_paper;

    case ColorConstant::silver_white:
        static constexpr const Color silver_white(29, 29, 30);
        return silver_white;

    case ColorConstant::rich_black:
        static constexpr const Color rich_black(0, 0, 2);
        return rich_black;

    default:
        return Color(k);
    }
}


using PaletteBank = int;
constexpr PaletteBank available_palettes = 4;
constexpr PaletteBank palette_count = 16;

static PaletteBank palette_counter = available_palettes;


static u8 screen_pixelate_amount = 0;


Color adjust_warmth(const Color& c, int amount)
{
    auto ret = c;
    ret.r_ = clamp(c.r_ + amount, 0, 31);
    ret.b_ = clamp(c.b_ - amount, 0, 31);

    return ret;
}



ColorConstant
grayscale_shader(ShaderPalette palette, ColorConstant k, int arg, int index)
{
    return Color::from_bgr_hex_555(
               blend(Color(k), Color(k).grayscale(), (u8)arg))
        .hex();
}



ColorConstant
contrast_shader(ShaderPalette palette, ColorConstant k, int arg, int index)
{
    const Float f = (259.f * ((s8)arg + 255)) / (255 * (259 - (s8)arg));

    const Color c(k);

    const auto r = clamp(f * (Color::upsample(c.r_) - 128) + 128, 0.f, 255.f);
    const auto g = clamp(f * (Color::upsample(c.g_) - 128) + 128, 0.f, 255.f);
    const auto b = clamp(f * (Color::upsample(c.b_) - 128) + 128, 0.f, 255.f);

    return Color(
               Color::downsample(r), Color::downsample(g), Color::downsample(b))
        .hex();
}



ColorConstant
passthrough_shader(ShaderPalette palette, ColorConstant k, int arg, int index)
{
    return k;
}



static Platform::Screen::Shader shader = passthrough_shader;
static int shader_argument = 0;



void Platform::Screen::set_shader(Shader shader)
{
    ::shader = shader;
    set_shader_argument(0);
}



Color agb_color_correction(const Color& c)
{
    if (not color_correction_lut) {
        return c;
    }

#define COLOR_LUT_SIZE 32

    // Calculate the index into the flat LUT array
    int index = (c.r_ * COLOR_LUT_SIZE * COLOR_LUT_SIZE +
                 c.g_ * COLOR_LUT_SIZE + c.b_) *
                3;

    // Access the RGB channels from the LUT
    u8 r_value = color_correction_lut[index + 0];
    u8 g_value = color_correction_lut[index + 1];
    u8 b_value = color_correction_lut[index + 2];

    return Color(r_value, g_value, b_value);
}



static Color invoke_shader(const Color& c, ShaderPalette palette, int index)
{
    return agb_color_correction(shader(std::move(palette),
                                       c.hex(),
                                       std::move(shader_argument),
                                       std::move(index)));
}



EWRAM_DATA u16 sprite_palette[16];
EWRAM_DATA u16 tilesheet_0_palette[16];
EWRAM_DATA u16 tilesheet_1_palette[16];
EWRAM_DATA u16 overlay_palette[16];
EWRAM_DATA u16 background_palette[16];
EWRAM_DATA u16 tilesheet_0_darkened_palette[16];
EWRAM_DATA u16 custom_flag_palette[16];



static void
init_palette(const u16* palette_input, u16* palette, ShaderPalette p)
{
    for (int i = 0; i < 16; ++i) {
        palette[i] =
            invoke_shader(Color::from_bgr_hex_555(palette_input[i]), p, i)
                .bgr_hex_555();
    }
}



void init_darkened_palette();



void Platform::Screen::set_shader_argument(int arg)
{
    shader_argument = arg;

    init_palette(current_overlay_texture->palette_data_,
                 overlay_palette,
                 ShaderPalette::overlay);

    init_palette(current_spritesheet->palette_data_,
                 sprite_palette,
                 ShaderPalette::spritesheet);

    init_palette(current_tilesheet0->palette_data_,
                 tilesheet_0_palette,
                 ShaderPalette::tile0);

    init_palette(current_tilesheet1->palette_data_,
                 tilesheet_1_palette,
                 ShaderPalette::tile1);

    init_palette(current_background->palette_data_,
                 background_palette,
                 ShaderPalette::background);

    init_darkened_palette();
}



// For the purpose of saving cpu cycles. The color_mix function scans a list of
// previous results, and if one matches the current blend parameters, the caller
// will set the locked_ field to true, and return the index of the existing
// palette bank. Each call to display() unlocks all of the palette infos.
static struct PaletteInfo
{
    ColorConstant color_ = ColorConstant::null;
    u8 blend_amount_ = 0;
    bool locked_ = false;
    bool used_ = false;
} palette_info[palette_count] = {};



// Perform a color mix between the spritesheet palette bank (bank zero), and
// return the palette bank where the resulting mixture is stored. We can only
// display 12 mixed colors at a time, because the first four banks are in use.
static PaletteBank color_mix(ColorConstant k, u8 amount)
{
    // if (UNLIKELY(color_mix_disabled)) {
    //     return 0;
    // }

    for (PaletteBank palette = available_palettes; palette < palette_count;
         ++palette) {
        auto& info = palette_info[palette];
        if (info.color_ == k and info.blend_amount_ == amount) {
            info.locked_ = true;
            info.used_ = true;
            return palette;
        }
    }

    // Skip over any palettes that are in use
    while (palette_info[palette_counter].locked_) {
        if (UNLIKELY(palette_counter == palette_count)) {
            return 0;
        }
        ++palette_counter;
    }

    if (UNLIKELY(palette_counter == palette_count)) {
        // Ok, so in this instance, we ran out of unused palettes, but there may
        // be a palette that was used in the last frame, and which has not yet
        // been referenced while rendering the current frame. Unlock that
        // palette and use it. Note that doing this could cause sprites to
        // flicker, because there's still a sprite in the OAM front buffer using
        // the locked-but-not-used palette bank. We would not need two booleans,
        // locked_ and used_, in the table for keeping state if we simply used a
        // palette back buffer, but adding a back buffer for the OAM palettes
        // complicates the logic in other ways, uses more memory, and would
        // require us to copy the whole sprite palette bank into VRAM on every
        // frame. Because we typically aren't exhausting our supply of ColorMix
        // palettes anyway, we don't do double buffering for sprite palettes,
        // and instead, consciously avoid writing to palette banks used by
        // sprites in the previous frame, unless we really run out of palettes
        // (which is a rare case, so not worth the cost of double buffering,
        // IMO).
        if (auto salvaged_palette = [&] {
                for (PaletteBank palette = available_palettes; palette < 16;
                     ++palette) {
                    auto& info = palette_info[palette];
                    if (info.locked_ and not info.used_) {
                        return palette;
                    }
                }
                return 0;
            }()) {
            palette_counter = salvaged_palette;
        } else {
            return 0;
        }
    }

    const auto c = invoke_shader(real_color(k), ShaderPalette::tile0, 0);

    if (amount not_eq 255) {
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(MEM_PALETTE[i]);
            const u32 index = 16 * palette_counter + i;
            MEM_PALETTE[index] = Color(fast_interpolate(c.r_, from.r_, amount),
                                       fast_interpolate(c.g_, from.g_, amount),
                                       fast_interpolate(c.b_, from.b_, amount))
                                     .bgr_hex_555();
        }
    } else {
        for (int i = 0; i < 16; ++i) {
            const u32 index = 16 * palette_counter + i;
            // No need to actually perform the blend operation if we're mixing
            // in 100% of the other color.
            MEM_PALETTE[index] = c.bgr_hex_555();
        }
    }

    palette_info[palette_counter] = {k, amount, true, true};

    return palette_counter++;
}


static struct DynamicTextureMapping
{
    bool reserved_ = false;
    bool dirty_ = false;
    u16 spritesheet_offset_ = 0;
} dynamic_texture_mappings[Platform::dynamic_texture_count];


u16 find_dynamic_mapping(u16 virtual_index)
{
    // FIXME: this code will not work for 32x32 pixel sprites yet!
    for (int i = 0; i < Platform::dynamic_texture_count; ++i) {
        if (virtual_index == dynamic_texture_mappings[i].spritesheet_offset_ and
            dynamic_texture_mappings[i].reserved_) {
            return i * 2;
        }
    }
    warning("mapping not found");
    return 0;
}



static Vec2<s32> get_view_center(const Platform::Screen& screen)
{
    return screen.get_view().int_center().cast<s32>();
}



static bool is_onscreen(const Platform::Screen& screen, const Vec2<Fixnum>& pos)
{
    const auto view_center = get_view_center(screen);
    const auto view_half_extent = screen.size().cast<s32>() / s32(2);
    Vec2<s32> view_br = {view_center.x + view_half_extent.x * 2,
                         view_center.y + view_half_extent.y * 2};

    // FIXME: check y range! Due to the gba's hardware wrapping, I didn't
    // realize that certain layers and sprites were actually wrapped over in the
    // y-direction, which makes the view bounds check difficult. Need to fix the
    // y coordinates!!!

    return pos.x.as_integer() > view_center.x - 32 and
           pos.x.as_integer() < view_br.x + 32;
}



void Platform::Screen::draw_batch(TextureIndex t,
                                  const Buffer<Vec2<s32>, 64>& coords,
                                  const SpriteBatchOptions& opts)
{
    auto view_center = get_view_center(*this);

    const auto view_half_extent = size().cast<s32>() / s32(2);
    Vec2<s32> view_br = {view_center.x + view_half_extent.x * 2,
                         view_center.y + view_half_extent.y * 2};

    if (opts.position_absolute_) {
        view_center.x = 0;
        view_center.y = 0;
        view_br.x = 240;
        view_br.y = 160;
    }

    int tx_scale = 8;

    auto shape = ATTR0_TALL;
    auto size = ATTR1_SIZE_16;
    switch (opts.sz_) {
    case Sprite::Size::w16_h32:
        shape = ATTR0_TALL;
        size = ATTR1_SIZE_32;
        tx_scale = 8;
        break;

    case Sprite::Size::w32_h32:
        shape = ATTR0_SQUARE;
        size = ATTR1_SIZE_32;
        tx_scale = 16;
        break;

    case Sprite::Size::w16_h16:
        shape = ATTR0_SQUARE;
        size = ATTR1_SIZE_16;
        tx_scale = 4;
        break;

    case Sprite::Size::w8_h8:
        shape = ATTR0_SQUARE;
        size = ATTR1_SIZE_8;
        tx_scale = 1;
        break;
    }

    const auto target_index = 2 + t * tx_scale;

    for (auto& c : coords) {
        if (UNLIKELY(oam_write_index == oam_count)) {
            return;
        }

        if (not(c.x > view_center.x - 32 and c.x < view_br.x + 32)) {
            // Offscreen in x direction.
            continue;
        }

        auto oa = object_attribute_back_buffer + oam_write_index;
        if (opts.alpha_ not_eq Sprite::Alpha::translucent) {
            oa->attribute_0 = ATTR0_COLOR_16 | shape;
        } else {
            oa->attribute_0 = ATTR0_COLOR_16 | shape | ATTR0_BLEND;
        }

        oa->attribute_1 = size;
        auto abs_position = c - view_center;

        oa->attribute_0 &= (0xff00 & ~((1 << 8) | (1 << 9)));
        oa->attribute_0 |= abs_position.y & 0x00ff;

        oa->attribute_1 |= abs_position.x & 0x01ff;

        oa->attribute_2 = target_index;
        oa->attribute_2 |= 0; // palette bank
        oa->attribute_2 |= ATTR2_PRIORITY(1);
        oam_write_index += 1;
    }
}



void Platform::Screen::draw(const Sprite& spr)
{
    if (UNLIKELY(spr.get_alpha() == Sprite::Alpha::transparent)) {
        return;
    }

    if (not is_onscreen(*this, spr.get_position())) {
        return;
    }


    const auto& mix = spr.get_mix();


    auto pb = [&]() -> PaletteBank {
        if (UNLIKELY(mix.color_ not_eq ColorConstant::null)) {
            if (const auto pal_bank = color_mix(mix.color_, mix.amount_)) {
                return ATTR2_PALBANK(pal_bank);
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }();

    if (spr.palette()) {
        pb = ATTR2_PALBANK(spr.palette());
    }

    auto draw_sprite = [&](int tex_off,
                           int x_off,
                           int scale,
                           int shape,
                           int size) {
        if (UNLIKELY(oam_write_index == oam_count)) {
            return;
        }
        const auto position =
            ivec(spr.get_position()) - spr.get_origin().cast<s32>();

        const auto view_center = get_view_center(*this);
        auto oa = object_attribute_back_buffer + oam_write_index;
        if (spr.get_alpha() not_eq Sprite::Alpha::translucent) {
            oa->attribute_0 = ATTR0_COLOR_16 | shape;
        } else {
            oa->attribute_0 = ATTR0_COLOR_16 | shape | ATTR0_BLEND;
        }
        oa->attribute_1 = size; // clear attr1

        auto abs_position = position - view_center;

        oa->attribute_0 &= (0xff00 & ~((1 << 8) | (1 << 9))); // clear attr0

        if (spr.get_rotation() or spr.get_scale().x or spr.get_scale().y) {
            if (affine_transform_write_index not_eq affine_transform_limit) {
                auto& affine =
                    affine_transform_back_buffer[affine_transform_write_index];

                if (spr.get_rotation() and
                    (spr.get_scale().x or spr.get_scale().y)) {
                    affine.rot_scale(spr.get_rotation(),
                                     spr.get_scale().x,
                                     spr.get_scale().y);
                } else if (spr.get_rotation()) {
                    affine.rotate(spr.get_rotation());
                } else {
                    affine.scale(spr.get_scale().x, spr.get_scale().y);
                }

                oa->attribute_0 |= 1 << 8;
                oa->attribute_0 |= 1 << 9;

                abs_position.x -= 8;
                abs_position.y -= 16;

                oa->attribute_1 |= affine_transform_write_index << 9;

                affine_transform_write_index += 1;
            }
        } else {
            const auto& flip = spr.get_flip();
            oa->attribute_1 |= ((int)flip.y << 13);
            oa->attribute_1 |= ((int)flip.x << 12);
        }

        oa->attribute_0 |= abs_position.y & 0x00ff;

        if (not mix.amount_ and screen_pixelate_amount not_eq 0) {

            oa->attribute_0 |= ATTR0_MOSAIC;
        }

        oa->attribute_1 |= (abs_position.x + x_off) & 0x01ff;

        auto ti = spr.get_texture_index();
        // if (w16_h32_index > 125) {
        //     ti = find_dynamic_mapping(w16_h32_index);
        //     if (scale == 16) {
        //         ti /= 2;
        //     }
        // }
        const auto target_index = 2 + ti * scale + tex_off;
        oa->attribute_2 = target_index;
        oa->attribute_2 |= pb;
        oa->attribute_2 |= ATTR2_PRIORITY(spr.get_priority());
        oam_write_index += 1;
    };

    switch (spr.get_size()) {
    case Sprite::Size::w32_h32:
        // In order to fit the spritesheet into VRAM, the game draws
        // sprites in 32x16 pixel chunks, although several sprites are
        // really 32x32. 32x16 is easy to meta-tile for 1D texture
        // mapping, and a 32x32 sprite can be represented as two 32x16
        // sprites. If all sprites were 32x32, the spritesheet would
        // not fit into the gameboy advance's video memory. 16x16
        // would be even more compact, but would be inconvenient to
        // work with from a art/drawing perspective. Maybe I'll write
        // a script to reorganize the spritesheet into a Nx16 strip,
        // and metatile as 2x2 gba tiles... someday.

        // When a sprite is flipped, each of the individual 32x16 strips are
        // flipped, and then we need to swap the drawing X-offsets, so that the
        // second strip will be drawn first.
        if (not spr.get_flip().x) {
            draw_sprite(0, 0, 16, ATTR0_TALL, ATTR1_SIZE_32);
            draw_sprite(8, 16, 16, ATTR0_TALL, ATTR1_SIZE_32);

        } else {
            draw_sprite(0, 16, 16, ATTR0_TALL, ATTR1_SIZE_32);
            draw_sprite(8, 0, 16, ATTR0_TALL, ATTR1_SIZE_32);
        }

        break;

    case Sprite::Size::w16_h32:
        draw_sprite(0, 0, 8, ATTR0_TALL, ATTR1_SIZE_32);
        break;

    case Sprite::Size::w16_h16:
        draw_sprite(0, 0, 4, ATTR0_SQUARE, ATTR1_SIZE_16);
        break;

    case Sprite::Size::w8_h8:
        draw_sprite(0, 0, 1, ATTR0_SQUARE, ATTR1_SIZE_8);
        break;
    }
}



void Platform::load_overlay_chunk(TileDesc dst,
                                  TileDesc src,
                                  u16 count,
                                  const char* image_file)
{
    const u8* image_data = (const u8*)current_overlay_texture->tile_data_;

    if (image_file) {
        for (auto& info : overlay_textures) {
            if (str_cmp(image_file, info.name_) == 0) {
                image_data = (const u8*)info.tile_data_;
                break;
            }
        }
    }

    u8* overlay_vram_base_addr = (u8*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0];

    const auto chunk_size = vram_tile_size() * count;

    memcpy32(overlay_vram_base_addr + vram_tile_size() * dst,
             image_data + vram_tile_size() * src,
             chunk_size / 4); // memcpy32 copy amount is in units of words.
}



static void map_dynamic_textures()
{
    for (int i = 0; i < Platform::dynamic_texture_count; ++i) {
        auto& mapping = dynamic_texture_mappings[i];
        if (mapping.dirty_) {
            // Ok, so now, we want to perform a copy from ROM into the reserved
            // VRAM for our dynamic texture.
            const auto offset = mapping.spritesheet_offset_;
            const u8* image_data = (const u8*)current_spritesheet->tile_data_;
            u8* spr_vram_base_addr = (u8*)&MEM_TILE[4][1];

            constexpr auto single_tile_size = vram_tile_size();
            constexpr auto w32_h16_chunk_size = single_tile_size * 8;
            constexpr auto w32_h32_chunk_size = w32_h16_chunk_size * 2;

            memcpy32(spr_vram_base_addr + w32_h16_chunk_size * (i * 2),
                     image_data + w32_h16_chunk_size * offset,
                     // NOTE: we always copy in 32x32 chunks. Our dynamic tiles
                     // need to support either 32x16 or 32x32 chunks, and we do
                     // not know enough about how the sprites are going to
                     // leverage the video memory to know whether the remapped
                     // tiles will be drawn with 32x16 or 32x32 pixel sprites.
                     w32_h32_chunk_size / 4);

            mapping.dirty_ = false;
        }
    }
}


extern s16 parallax_table[280];
extern s16 vertical_parallax_table[280];


static u16 x0_scroll = 0;
static u16 y0_scroll = 0;
static u16 x3_scroll = 0;
static u16 y3_scroll = 0;


static const int tile_reserved_count = 9;
static const int tile_mapping_slots = 111 - tile_reserved_count;

using TileMappings = s16[tile_mapping_slots];

EWRAM_DATA static TileMappings tile0_mappings = {0};
EWRAM_DATA static TileMappings tile1_mappings = {0};



void Platform::clear_tile0_mappings()
{
    for (auto& mapping : tile0_mappings) {
        mapping = 0;
    }
}



void Platform::clear_tile1_mappings()
{
    for (auto& mapping : tile1_mappings) {
        mapping = 0;
    }
}



// Mapped tile -> tile enumeration
static TileDesc translate_tile0_index(TileDesc index)
{
    if (index < tile_reserved_count) {
        return index;
    }

    auto adjusted_index = index - tile_reserved_count;
    if (adjusted_index < tile_mapping_slots and
        tile0_mappings[adjusted_index]) {
        return tile0_mappings[adjusted_index];
    }

    return index;
}



static TileDesc translate_tile1_index(TileDesc index)
{
    if (index < tile_reserved_count) {
        return index;
    }

    auto adjusted_index = index - tile_reserved_count;
    if (adjusted_index < tile_mapping_slots and
        tile1_mappings[adjusted_index]) {
        return tile1_mappings[adjusted_index];
    }

    return index;
}



static TileDesc map_tile_chunk(TileMappings mappings,
                               TileDesc src,
                               u8* src_image_data,
                               int dest_charblock)
{
    if (src == 0) {
        return 0;
    }

    if (src < tile_reserved_count) {
        return src;
    }

    int tile_data_start = 128;

    for (int i = 0; i < tile_mapping_slots; ++i) {
        if (mappings[i] == src) {
            return i + tile_reserved_count;
        }
    }

    int i = 0;
    // FIXME: remove this code?! Unreachable?
    for (; i < tile_mapping_slots; ++i) {
        if (mappings[i] == 0) {
            mappings[i] = src;
            break;
        }
    }

    if (i == tile_mapping_slots) {
        // Out of tile mappings!
        return 112;
    }

    src -= 1;
    i += tile_reserved_count;

    u8* p_dest = ((u8*)&MEM_SCREENBLOCKS[dest_charblock][0]) +
                 (i) * (vram_tile_size() * 4);

    u8* p_src = ( // (u8*)current_tilesheet0->tile_data_
                    src_image_data) +
                ((src) + tile_data_start) * (vram_tile_size() * 4);

    memcpy16(p_dest, p_src, (vram_tile_size() * 4) / 2);

    return i;
}



TileDesc Platform::map_tile0_chunk(TileDesc src)
{
    return map_tile_chunk(tile0_mappings,
                          src,
                          (u8*)current_tilesheet0->tile_data_,
                          sbb_t0_texture);
}



TileDesc Platform::map_tile1_chunk(TileDesc src)
{
    return map_tile_chunk(tile1_mappings,
                          src,
                          (u8*)current_tilesheet1->tile_data_,
                          sbb_t1_texture);
}



void Platform::blit_t0_erase(u16 index)
{
    u8* p =
        ((u8*)&MEM_SCREENBLOCKS[sbb_t0_texture][0]) + index * vram_tile_size();

    memset16(p, 0, vram_tile_size() / 2);
}



void Platform::blit_t1_erase(u16 index)
{
    u8* p =
        ((u8*)&MEM_SCREENBLOCKS[sbb_t1_texture][0]) + index * vram_tile_size();

    memset16(p, 0, vram_tile_size() / 2);
}



void Platform::blit_t0_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
    auto data = (u8*)current_tilesheet0->tile_data_;
    data += from_index * vram_tile_size();

    u8* p = ((u8*)&MEM_SCREENBLOCKS[sbb_t0_texture][0]) +
            to_index * vram_tile_size();

    if (hard) {
        memcpy16(p, data, vram_tile_size() / 2);
    } else {
        blit_tile((u16*)p, (u16*)data);
    }
}



void Platform::blit_t1_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
    auto data = (u8*)current_tilesheet1->tile_data_;
    data += from_index * vram_tile_size();

    u8* p = ((u8*)&MEM_SCREENBLOCKS[sbb_t1_texture][0]) +
            to_index * vram_tile_size();

    if (hard) {
        memcpy16(p, data, vram_tile_size() / 2);
    } else {
        blit_tile((u16*)p, (u16*)data);
    }
}



void Platform::overwrite_overlay_tile(u16 index, const EncodedTile& t)
{
    u8* p = ((u8*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0]) +
            index * (vram_tile_size());

    memcpy16(p, &t, ((sizeof t) / 4) / 2);
}



void Platform::overwrite_t0_tile(u16 index, const EncodedTile& t)
{
    u8* p = ((u8*)&MEM_SCREENBLOCKS[sbb_t0_texture][0]) +
            index * (vram_tile_size() * 4);

    memcpy16(p, &t, (sizeof t) / 2);
}



void Platform::overwrite_t1_tile(u16 index, const EncodedTile& t)
{
    u8* p = ((u8*)&MEM_SCREENBLOCKS[sbb_t1_texture][0]) +
            index * (vram_tile_size() * 4);

    memcpy16(p, &t, (sizeof t) / 2);
}



void Platform::overwrite_sprite_tile(u16 index, const EncodedTile& t)
{
    // NOTE: Sprites occupy 16x32 pixels, i.e. 8 8x8 pixel tiles.
    u8* p = ((u8*)&MEM_TILE[4][1]) + index * (vram_tile_size() * 8);

    memcpy16(p, &t, (sizeof t) / 2);
}



Platform::TilePixels Platform::extract_tile(Layer layer, u16 tile)
{
    TilePixels result;

    switch (layer) {
    case Layer::map_1_ext:
    case Layer::map_0_ext:
    case Layer::map_1:
    case Layer::map_0: {
        auto mem =
            (const u8*)((layer == Layer::map_0 or layer == Layer::map_0_ext)
                            ? current_tilesheet0->tile_data_
                            : current_tilesheet1->tile_data_) +
            vram_tile_size() * 4 * tile; // 2x2 meta tiles
        for (int y = 0; y < 8; ++y) {
            for (int x = 0; x < 8; ++x) {
                auto index = x + y * 8;
                auto idx1 = index / 2;
                if (x % 2) {
                    result.data_[x][y] = (mem[idx1] & 0xf0) >> 4;
                } else {
                    result.data_[x][y] = mem[idx1] & 0x0f;
                }
                auto idx2 = index / 2 + vram_tile_size();
                if (x % 2) {
                    result.data_[x + 8][y] = (mem[idx2] & 0xf0) >> 4;
                } else {
                    result.data_[x + 8][y] = mem[idx2] & 0x0f;
                }
                auto idx3 = index / 2 + 2 * vram_tile_size();
                if (x % 2) {
                    result.data_[x][y + 8] = (mem[idx3] & 0xf0) >> 4;
                } else {
                    result.data_[x][y + 8] = mem[idx3] & 0x0f;
                }
                auto idx4 = index / 2 + 3 * vram_tile_size();
                if (x % 2) {
                    result.data_[x + 8][y + 8] = (mem[idx4] & 0xf0) >> 4;
                } else {
                    result.data_[x + 8][y + 8] = mem[idx4] & 0x0f;
                }
            }
        }
        break;
    }

    case Layer::background:
        fatal("unimplemented extract_tile for background layer");

    case Layer::overlay:
        auto mem = (const u8*)(current_overlay_texture->tile_data_) +
                   vram_tile_size() * tile;
        for (int y = 0; y < 8; ++y) {
            for (int x = 0; x < 8; ++x) {
                auto index = x + y * 8;
                auto idx1 = index / 2;
                if (x % 2) {
                    result.data_[x][y] = (mem[idx1] & 0xf0) >> 4;
                } else {
                    result.data_[x][y] = mem[idx1] & 0x0f;
                }
            }
        }
        break;
    }

    return result;
}



Platform::EncodedTile Platform::encode_tile(u8 tile_data[16][16])
{
    EncodedTile t;
    using Buf = Buffer<u8, 128>;
    Buf buffer(Buf::SkipZeroFill{});

    for (int i = 0; i < 8; ++i) {
        for (int j = 0; j < 8; ++j) {
            if (j % 2) {
                buffer.back() |= tile_data[j][i] << 4;
            } else {
                buffer.push_back(tile_data[j][i] & 0xff);
            }
        }
    }

    for (int i = 0; i < 8; ++i) {
        for (int j = 8; j < 16; ++j) {
            if (j % 2) {
                buffer.back() |= tile_data[j][i] << 4;
            } else {
                buffer.push_back(tile_data[j][i] & 0xff);
            }
        }
    }

    for (int i = 8; i < 16; ++i) {
        for (int j = 0; j < 8; ++j) {
            if (j % 2) {
                buffer.back() |= tile_data[j][i] << 4;
            } else {
                buffer.push_back(tile_data[j][i] & 0xff);
            }
        }
    }

    for (int i = 8; i < 16; ++i) {
        for (int j = 8; j < 16; ++j) {
            if (j % 2) {
                buffer.back() |= tile_data[j][i] << 4;
            } else {
                buffer.push_back(tile_data[j][i] & 0xff);
            }
        }
    }


    memcpy32(t.bytes_, buffer.data(), 128 / 4);

    return t;
}



using OptDmaBufferData = std::array<u16, 161>;
EWRAM_DATA Optional<DynamicMemory<OptDmaBufferData>> opt_dma_buffer_;
EWRAM_DATA int dma_effect_params[3];



static void vblank_circle_effect_isr()
{
    // TODO: re-organize parallax_table and vertical_parallax_table,
    // interleaving the data, so that we only need one DMA
    // controller. Fix this someday, when running out of dma channels.
    DMA_TRANSFER(
        (volatile short*)0x4000014, &parallax_table[1], 1, 0, DMA_HDMA);
    DMA_TRANSFER((volatile short*)0x4000016,
                 &vertical_parallax_table[1],
                 1,
                 3,
                 DMA_HDMA);

    DMA_TRANSFER(&REG_WIN0H, (*opt_dma_buffer_)->data(), 1, 2, DMA_HDMA);
}



static void vblank_full_transfer_scroll_isr()
{
    DMA_TRANSFER(
        (volatile short*)0x4000014, &parallax_table[1], 1, 0, DMA_HDMA);
    DMA_TRANSFER((volatile short*)0x4000016,
                 &vertical_parallax_table[1],
                 1,
                 3,
                 DMA_HDMA);

    DMA_TRANSFER(&REG_WIN0H, &vertical_parallax_table[1], 1, 2, 0);
}



static void vblank_horizontal_transfer_scroll_isr()
{
    DMA_TRANSFER(
        (volatile short*)0x4000014, &parallax_table[1], 1, 0, DMA_HDMA);

    // Disable prior transfers.
    DMA_TRANSFER(
        (volatile short*)0x4000016, &vertical_parallax_table[1], 1, 3, 0);
    DMA_TRANSFER(&REG_WIN0H, &vertical_parallax_table[1], 1, 2, 0);
}



void no_op_task()
{
}



void (*vblank_dma_callback)() = vblank_full_transfer_scroll_isr;
void (*vblank_task)() = no_op_task;



Platform::TaskPointer Platform::set_background_task(Platform::TaskPointer task)
{
    TaskPointer ret = vblank_task;

    if (task == nullptr) {
        vblank_task = no_op_task;
    } else {
        vblank_task = task;
    }

    if (ret == no_op_task) {
        return nullptr;
    }

    return ret;
}



void Platform::Screen::clear()
{
    if (not canary_check()) {
        longjmp(stack_overflow_resume_context, 1);
    }

    // VSync
    VBlankIntrWait();

    if (opt_dma_buffer_ and vblank_dma_callback == vblank_circle_effect_isr) {
        // NOTE: this circle sidelength calculation is too expensive to fit in
        // the vblank interrupt handler, because it causes missed audio timer
        // interrupts and an unpleasant screeching sound.
        if (get_gflag(GlobalFlag::iris_effect_mode)) {
            memset16((*opt_dma_buffer_)->data(), 0, 160);
        }
        win_circle((*opt_dma_buffer_)->data(),
                   dma_effect_params[1],
                   dma_effect_params[2],
                   dma_effect_params[0]);
    }


    if (get_gflag(GlobalFlag::palette_sync)) {

        memcpy32(MEM_PALETTE, sp_palette_back_buffer, 16);

        memcpy32(MEM_BG_PALETTE, bg_palette_back_buffer,
                 24); // word count

        memcpy32(
            MEM_BG_PALETTE + 16 * 11,
            bg_palette_back_buffer + 16 * 11,
            16); // 16 words, both the background palette and the flag palette.

        set_gflag(GlobalFlag::palette_sync, false);
    } else if (get_gflag(GlobalFlag::partial_palette_sync)) {

        memcpy32(MEM_BG_PALETTE, bg_palette_back_buffer, 8);
        memcpy32(MEM_BG_PALETTE + 32, bg_palette_back_buffer + 32, 8);
        memcpy32(&MEM_BG_PALETTE[16 * 11], &bg_palette_back_buffer[16 * 11], 8);

        set_gflag(GlobalFlag::partial_palette_sync, false);
    }

    // We want to do the dynamic texture remapping near the screen clear, to
    // reduce tearing. Most of the other changes that we make to vram, like the
    // overlay tiles and OAM, are double-buffered, so the tearing is less
    // noticable if we perform the copies further from the site of the vsync.
    map_dynamic_textures();

    auto view_offset = view_.get_center().cast<s32>();
    BG0_X_SCROLL = x0_scroll + view_offset.x;
    BG0_Y_SCROLL = y0_scroll + view_offset.y;

    BG3_X_SCROLL = x3_scroll + view_offset.x;
    BG3_Y_SCROLL = y3_scroll + view_offset.y;
}


void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
    switch (layer) {
    case Layer::background:
        break;

    case Layer::overlay:
        set_overlay_origin(x, y);
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
        x0_scroll = x;
        y0_scroll = y;
        break;

    case Layer::map_1_ext:
    case Layer::map_1:
        x3_scroll = x;
        y3_scroll = y;
        break;
    }
}


Vec2<u16> Platform::get_scroll(Layer layer)
{
    switch (layer) {
    case Layer::background:
    case Layer::overlay:
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
        return {x0_scroll, y0_scroll};

    case Layer::map_1_ext:
    case Layer::map_1:
        return {x3_scroll, y3_scroll};
    }

    return {};
}


[[noreturn]] static void restart()
{
    // NOTE: I am clearing almost everything, because when I did not clear all
    // of these memory segments, something seemed to be interfering with gameboy
    // player unlocking.
    RegisterRamReset(RESET_VRAM | RESET_PALETTE | RESET_OAM | RESET_SIO |
                     RESET_SOUND | RESET_OTHER);
    SoftReset(ROM_RESTART), __builtin_unreachable();
}



static void keypad_isr()
{
    restart();
}



void Platform::Screen::display()
{
    if (overlay_back_buffer_changed) {
        overlay_back_buffer_changed = false;

        // If the overlay has not scrolled, then we do not need to bother with
        // the lower twelve rows of the overlay tile data, because the screen is
        // twenty tiles tall. This hack could be problematic if someone scrolls
        // the screen a lot without editing the overlay.
        if (overlay_y == 0) {
            memcpy32(MEM_SCREENBLOCKS[sbb_overlay_tiles],
                     overlay_back_buffer,
                     (sizeof(u16) * (21 * 32)) / 4);
        } else {
            memcpy32(MEM_SCREENBLOCKS[sbb_overlay_tiles],
                     overlay_back_buffer,
                     (sizeof overlay_back_buffer) / 4);
        }
    }

    for (u32 i = oam_write_index; i < last_oam_write_index; ++i) {
        // Disable affine transform for unused sprite
        object_attribute_back_buffer[i].attribute_0 &= ~((1 << 8) | (1 << 9));
        object_attribute_back_buffer[i].attribute_1 = 0;

        object_attribute_back_buffer[i].attribute_0 |= attr0_mask::disabled;
    }

    for (u32 i = affine_transform_write_index;
         i < last_affine_transform_write_index;
         ++i) {

        auto& affine = affine_transform_back_buffer[i];
        affine.pa() = 0;
        affine.pb() = 0;
        affine.pc() = 0;
        affine.pd() = 0;
    }

    // I noticed less graphical artifacts when using a back buffer. I thought I
    // would see better performance when writing directly to OAM, rather than
    // doing a copy later, but I did not notice any performance difference when
    // adding a back buffer.
    memcpy32(object_attribute_memory,
             object_attribute_back_buffer,
             (sizeof object_attribute_back_buffer) / 4);

    last_affine_transform_write_index = affine_transform_write_index;
    affine_transform_write_index = 0;

    last_oam_write_index = oam_write_index;
    oam_write_index = 0;
    palette_counter = available_palettes;

    for (auto& info : palette_info) {
        if (not info.used_) {
            info.locked_ = false;
        }
        info.used_ = false;
    }

    auto view_offset = view_.get_center().cast<s32>();

    // Depending on the amount of the background scroll, we want to mask off
    // certain parts of bg0 and bg3. The background tiles wrap when they scroll
    // a certain distance, and wrapping looks strange (although it might be
    // useful if you were making certain kinds of games, like some kind of
    // Civilization clone, but for BlindJump, it doesn't make sense to display
    // the wrapped area).

    if (not get_gflag(GlobalFlag::effect_window_mode)) {
        const s32 scroll_limit_x_max = 512 - size().x;
        // const s32 scroll_limit_y_max = 480 - size().y;
        if (view_offset.x > scroll_limit_x_max) {
            REG_WIN0H =
                (0 << 8) | (size().x - (view_offset.x - scroll_limit_x_max));
        } else if (view_offset.x < 0) {
            REG_WIN0H = ((view_offset.x * -1) << 8) | (0);
        } else {
            REG_WIN0H = (0 << 8) | (size().x);
        }

        REG_WIN0V = (0 << 8) | (size().y);
    }

    if (not get_gflag(GlobalFlag::parallax_clouds)) {
        BG1_X_SCROLL = view_offset.x * 0.3f;
        BG1_Y_SCROLL = view_offset.y * 0.3f;
    } else {
        if (not get_gflag(GlobalFlag::v_parallax)) {
            BG1_Y_SCROLL = view_offset.y / 2;
        }
    }
}



Vec2<u32> Platform::Screen::size() const
{
    static constexpr const Vec2<u32> gba_widescreen{240, 160};
    return gba_widescreen;
}


////////////////////////////////////////////////////////////////////////////////
// Platform
////////////////////////////////////////////////////////////////////////////////



static bool validate_tilemap_texture_size(size_t size)
{
    if (size > charblock_size) {
        PLATFORM.fatal("tileset exceeds charblock size");
        return false;
    }
    return true;
}



static bool validate_background_texture_size(size_t size)
{
    if (size > sizeof(ScreenBlock) * 2) {
        PLATFORM.fatal("background exceeds screenblock size x 2");
        return false;
    }
    return true;
}



static u16 get_map_tile_16p(u8 base, u16 x, u16 y, int palette)
{
    auto ref = [](u16 x_, u16 y_) { return x_ * 2 + y_ * 32 * 2; };

    auto screen_block = [&]() -> u16 { return base; }();

    return (MEM_SCREENBLOCKS[screen_block][0 + ref(x % 16, y)] &
            ~(SE_PALBANK_MASK)) /
           4;
}



static void set_map_tile_16p(u8 base, u16 x, u16 y, u16 tile_id, int palette)
{
    auto ref = [](u16 x_, u16 y_) { return x_ * 2 + y_ * 32 * 2; };

    auto screen_block = [&]() -> u16 {
        if (x > 15) {
            x %= 16;
            return base + 1;
        } else {
            return base;
        }
    }();

    MEM_SCREENBLOCKS[screen_block][0 + ref(x % 16, y)] =
        (tile_id * 4 + 0) | SE_PALBANK(palette);

    MEM_SCREENBLOCKS[screen_block][1 + ref(x % 16, y)] =
        (tile_id * 4 + 1) | SE_PALBANK(palette);

    MEM_SCREENBLOCKS[screen_block][0 + ref(x % 16, y) + 32] =
        (tile_id * 4 + 2) | SE_PALBANK(palette);

    MEM_SCREENBLOCKS[screen_block][1 + ref(x % 16, y) + 32] =
        (tile_id * 4 + 3) | SE_PALBANK(palette);
}


COLD static void set_map_tile(u8 base, u16 x, u16 y, u16 tile_id, int palette)
{
    // NOTE: The game's tiles are 32x24px in size. GBA tiles are each
    // 8x8. To further complicate things, the GBA's VRAM is
    // partitioned into 32x32 tile screenblocks, so some 32x24px tiles
    // cross over screenblocks in the vertical direction, and then the
    // y-offset is one-tile-greater in the lower quadrants.

    auto ref = [](u16 x_, u16 y_) { return x_ * 4 + y_ * 32 * 3; };

    // Tiles at y=10 need to jump across a gap between screen blocks.
    if (y == 10 and x > 7) {
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[base + 1][i + ref(x % 8, y)] =
                (tile_id * 12 + i) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[base + 1][i + ref(x % 8, y) + 32] =
                (tile_id * 12 + i + 4) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[base + 3][i + ref(x % 8, 0)] =
                (tile_id * 12 + i + 8) | SE_PALBANK(palette);
        }
        return;
    } else if (y == 10) {
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[base][i + ref(x, y)] =
                (tile_id * 12 + i) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[base][i + ref(x, y) + 32] =
                (tile_id * 12 + i + 4) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[base + 2][i + ref(x, 0)] =
                (tile_id * 12 + i + 8) | SE_PALBANK(palette);
        }
        return;
    }

    auto screen_block = [&]() -> u16 {
        if (x > 7 and y > 9) {
            x %= 8;
            y %= 10;
            return base + 3;
        } else if (y > 9) {
            y %= 10;
            return base + 2;
        } else if (x > 7) {
            x %= 8;
            return base + 1;
        } else {
            return base;
        }
    }();

    if (screen_block == base + 2 or screen_block == base + 3) {
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[screen_block][i + ref(x, y - 1) + 32] =
                (tile_id * 12 + i) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[screen_block][i + ref(x, y - 1) + 64] =
                (tile_id * 12 + i + 4) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[screen_block][i + ref(x, y - 1) + 96] =
                (tile_id * 12 + i + 8) | SE_PALBANK(palette);
        }
    } else {
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[screen_block][i + ref(x, y)] =
                (tile_id * 12 + i) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[screen_block][i + ref(x, y) + 32] =
                (tile_id * 12 + i + 4) | SE_PALBANK(palette);
        }
        for (u32 i = 0; i < 4; ++i) {
            MEM_SCREENBLOCKS[screen_block][i + ref(x, y) + 64] =
                (tile_id * 12 + i + 8) | SE_PALBANK(palette);
        }
    }
}


static u16 get_map_tile(u8 base, u16 x, u16 y, int palette)
{
    // I know that this code is confusing, sorry! See comment in set_map_tile().

    auto ref = [](u16 x_, u16 y_) { return x_ * 4 + y_ * 32 * 3; };

    if (y == 10 and x > 7) {
        return (MEM_SCREENBLOCKS[base][ref(x % 8, y)] &
                ~(SE_PALBANK(palette))) /
               12;
    } else if (y == 10) {
        return (MEM_SCREENBLOCKS[base][ref(x, y)] & ~(SE_PALBANK(palette))) /
               12;
    }

    auto screen_block = [&]() -> u16 {
        if (x > 7 and y > 9) {
            x %= 8;
            y %= 10;
            return base + 3;
        } else if (y > 9) {
            y %= 10;
            return base + 2;
        } else if (x > 7) {
            x %= 8;
            return base + 1;
        } else {
            return base;
        }
    }();

    if (screen_block == base + 2 or screen_block == base + 3) {
        return (MEM_SCREENBLOCKS[screen_block][ref(x, y - 1) + 32] &
                ~(SE_PALBANK(palette))) /
               12;
    } else {
        return (MEM_SCREENBLOCKS[screen_block][ref(x, y)] &
                ~(SE_PALBANK(palette))) /
               12;
    }
}



u16 Platform::get_tile(Layer layer, u16 x, u16 y)
{
    switch (layer) {
    case Layer::overlay:
        if (x > 31 or y > 31) {
            return 0;
        }
        return overlay_back_buffer[x + y * 32] & ~(SE_PALBANK_MASK);

    case Layer::background:
        if (x > 31 or y > 31) {
            return 0;
        }
        return MEM_SCREENBLOCKS[sbb_bg_tiles][x + y * 32] & ~(SE_PALBANK_MASK);

    case Layer::map_1_ext: {
        return translate_tile1_index(get_map_tile_16p(sbb_t1_tiles, x, y, 2));
    }

    case Layer::map_0_ext: {
        return translate_tile0_index(get_map_tile_16p(sbb_t0_tiles, x, y, 0));
    }

    case Layer::map_0:
        return get_map_tile(sbb_t0_tiles, x, y, 0);

    case Layer::map_1:
        return get_map_tile(sbb_t1_tiles, x, y, 2);
    }
    return 0;
}



void Platform::restart()
{
    ::restart();
}



static int watchdog_counter;



static void vblank_isr()
{
    vblank_dma_callback();
    vblank_task();

    watchdog_counter += 1;

    rumble_update();

    const auto ten_seconds = 600; // approx. 60 fps
    if (UNLIKELY(::watchdog_counter > ten_seconds)) {

        ::watchdog_counter = 0;

        if (not get_gflag(GlobalFlag::watchdog_disabled)) {
            ::__platform__->speaker().stop_music();

            if (not canary_check()) {
                REG_SOUNDCNT_X = 0; // Disable the sound chip.
                // on_stack_overflow() disables some interrupts, but we want to
                // disable additional ones, as we will not be leaving this
                // interrupt handler.
                irqDisable(IRQ_TIMER1 | IRQ_SERIAL);
                on_stack_overflow();
            } else {
                if (::__platform__ and ::unrecoverrable_error_callback) {
                    (*::unrecoverrable_error_callback)("game stalled");
                }
            }

            restart();
        }
    }

    if (not get_gflag(GlobalFlag::key_poll_called)) {
        Platform::Keyboard::KeyStates current_keys;
        poll_keys(current_keys);

        for (int i = 0; i < (int)Key::count; ++i) {
            if (current_keys[i]) {
                if (not ::missed_keys) {
                    ::missed_keys.emplace();
                    ::missed_keys->clear();
                }
                missed_keys->set(i, true);
            }
        }
    }

    set_gflag(GlobalFlag::key_poll_called, false);
}



void Platform::fatal(const char* msg)
{
    error(msg);

    ::__platform__->set_overlay_origin(0, 0);

    if (::__platform__ and ::unrecoverrable_error_callback) {
        (*::unrecoverrable_error_callback)(&*msg);
    }

    ::__platform__->screen().fade(0.f);

    const auto bkg_color = custom_color(0x007cbf);

    {
        int temp;
        DMA_TRANSFER((volatile short*)0x4000014, &temp, 1, 0, 0);
        DMA_TRANSFER((volatile short*)0x4000016, &temp, 1, 3, 0);
        DMA_TRANSFER(&REG_WIN0H, &vertical_parallax_table[1], 1, 2, 0);
    }
    vblank_dma_callback = no_op_task;
    window_init_default();

    irqDisable(IRQ_TIMER2 | IRQ_TIMER3 | IRQ_VBLANK);


    ::__platform__->screen().fade(1.f, bkg_color);
    ::__platform__->fill_overlay(0);
    ::__platform__->load_overlay_texture("overlay");
    ::__platform__->enable_glyph_mode(true);

    static constexpr const Text::OptColors text_colors{
        {custom_color(0xffffff), bkg_color}};

    static constexpr const Text::OptColors text_colors_inv{
        {text_colors->background_, text_colors->foreground_}};

    Text text({1, 1});
    text.append("fatal error:", text_colors_inv);

    Optional<Text> text2;


    Buffer<Text, 6> line_buffer;

    Optional<TextView> verbose_error;


    auto show_default_scrn = [&] {
        irqEnable(IRQ_VBLANK);

        ::__platform__->screen().clear();

        verbose_error.reset();

        ::__platform__->screen().display();
        ::__platform__->screen().clear();

        text2.emplace(OverlayCoord{1, 3});

        const auto msg_len = strlen(msg);
        if (msg_len > 28) {
            StringBuffer<28> temp;
            for (int i = 0; i < 21; ++i) {
                temp.push_back(msg[i]);
            }
            temp += "… (B)";
            text2->append(temp.c_str(), text_colors);
        } else {
            text2->append(msg, text_colors);
        }

        int offset = 0;

        auto render_line =
            [&](const char* line, int spacing, Text::OptColors colors) {
                line_buffer.emplace_back(OverlayCoord{1, u8(6 + offset)});
                line_buffer.back().append(line, colors);
                offset += spacing;
            };

        render_line("uart console available", 2, text_colors);
        render_line("link port        rs232 cable", 2, text_colors_inv);
        render_line("  SO  ---------------> RxD", 2, text_colors);
        render_line("  SI  ---------------> TxD", 2, text_colors);
        render_line("  GND ---------------> GND", 2, text_colors);
        render_line("    3.3 volts, 9600 baud    ", 2, text_colors_inv);

        Text text3({1, 18});
        text3.append("L+R+START+SELECT reset...", text_colors);

        ::__platform__->screen().display();

        irqDisable(IRQ_VBLANK);
    };

    show_default_scrn();

    lisp::init(PLATFORM.load_file("", "/lisp_symtab.dat"));

    auto show_verbose_msg = [&] {
        irqEnable(IRQ_VBLANK);

        ::__platform__->screen().clear();

        text2.reset();
        line_buffer.clear();

        ::__platform__->screen().display();
        ::__platform__->screen().clear();

        verbose_error.emplace();
        verbose_error->assign(msg, {1, 3}, {28, 14}, 0, text_colors);

        ::__platform__->screen().display();

        irqDisable(IRQ_VBLANK);
    };

    while (true) {

        if (auto line = ::__platform__->remote_console().readline()) {
            RemoteConsoleLispPrinter printer;

            lisp::BasicCharSequence seq(line->c_str());
            lisp::read(seq);
            lisp::eval(lisp::get_op(0));
            format(lisp::get_op(0), printer);

            lisp::pop_op();
            lisp::pop_op();

            ::__platform__->remote_console().printline(printer.fmt_.c_str());
        }

        ::__platform__->keyboard().poll();

        if (::__platform__->keyboard().down_transition<Key::action_2>()) {

            if (not verbose_error) {
                show_verbose_msg();
            } else {
                show_default_scrn();
            }
        }
    }
}


void Platform::set_overlay_origin(Float x, Float y)
{
    BG2_X_SCROLL = static_cast<s16>(x);
    BG2_Y_SCROLL = static_cast<s16>(y);
    overlay_y = y;
}


// Screen fades are cpu intensive. We want to skip any work that we possibly
// can.
static bool overlay_was_faded = false;


// TODO: May be possible to reduce tearing by deferring the fade until the
// Screen::display() call...
void Platform::Screen::fade(float amount,
                            ColorConstant k,
                            Optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{
    const u8 amt = amount * 255;

    // if (amt < 128) {
    //     color_mix_disabled = false;
    // } else {
    //     color_mix_disabled = true;
    // }

    if (amt == last_fade_amt and k == last_color and
        last_fade_include_sprites == include_sprites) {
        return;
    }

    last_fade_amt = amt;
    last_color = k;
    last_fade_include_sprites = include_sprites;

    const auto c = invoke_shader(real_color(k), ShaderPalette::tile0, 0);

    if (not base) {
        // Sprite palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(sprite_palette[i]);
            MEM_PALETTE[i] = blend(from, c, include_sprites ? amt : 0);
        }
        // Tile0 palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(tilesheet_0_palette[i]);
            MEM_BG_PALETTE[i] = blend(from, c, amt);
        }
        // Tile0 darkened palette
        for (int i = 0; i < 16; ++i) {
            auto from =
                Color::from_bgr_hex_555(tilesheet_0_darkened_palette[i]);
            MEM_BG_PALETTE[(9 * 16) + i] = blend(from, c, amt);
        }
        // Custom flag palette?
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(custom_flag_palette[i]);
            MEM_BG_PALETTE[16 * 12 + i] = blend(from, c, amt);
        }
        // Tile1 palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(tilesheet_1_palette[i]);
            MEM_BG_PALETTE[32 + i] = blend(from, c, amt);
        }
        // Custom flag palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(background_palette[i]);
            MEM_BG_PALETTE[16 * 11 + i] = blend(from, c, amt);
        }
        // Overlay palette
        if (include_overlay or overlay_was_faded) {
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(overlay_palette[i]);
                MEM_BG_PALETTE[16 + i] =
                    blend(from, c, include_overlay ? amt : 0);
            }
        }
        overlay_was_faded = include_overlay;
    } else {
        const auto bc =
            invoke_shader(real_color(*base), ShaderPalette::tile0, 0);
        for (int i = 0; i < 16; ++i) {
            MEM_PALETTE[i] = blend(bc, c, include_sprites ? amt : 0);
            MEM_BG_PALETTE[i] = blend(bc, c, amt);
            MEM_BG_PALETTE[32 + i] = blend(bc, c, amt);

            if (overlay_was_faded) {
                // FIXME!
                for (int i = 0; i < 16; ++i) {
                    auto from = Color::from_bgr_hex_555(overlay_palette[i]);
                    MEM_BG_PALETTE[16 + i] = blend(from, c, 0);
                }
                overlay_was_faded = false;
            }
        }
    }
}



bool Platform::Screen::fade_active() const
{
    return last_fade_amt > 0;
}



void Platform::Screen::schedule_fade(Float amount,
                                     ColorConstant k,
                                     bool include_sprites,
                                     bool include_overlay,
                                     bool include_background,
                                     bool include_tiles,
                                     bool dodge)
{
    const u8 amt = amount * 255;

    if (amt == last_fade_amt and k == last_color and
        last_fade_include_sprites == include_sprites) {
        return;
    }

    last_fade_amt = amt;
    last_color = k;
    last_fade_include_sprites = include_sprites;

    const auto c = invoke_shader(real_color(k), ShaderPalette::tile0, 0);


    set_gflag(GlobalFlag::palette_sync, true);


    // Sprite palette
    if (include_sprites or not dodge) {
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(sprite_palette[i]);
            sp_palette_back_buffer[i] =
                blend(from, c, include_sprites ? amt : 0);
        }
    }

    // Tile0 palette
    if (include_tiles or not dodge) {
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(tilesheet_0_palette[i]);
            bg_palette_back_buffer[i] = blend(from, c, include_tiles ? amt : 0);
        }
        // Tile0 darkened palette
        for (int i = 0; i < 16; ++i) {
            auto from =
                Color::from_bgr_hex_555(tilesheet_0_darkened_palette[i]);
            MEM_BG_PALETTE[(9 * 16) + i] = blend(from, c, amt);
        }
    }

    // Custom flag/tile/sprite palette:
    if (include_tiles or not dodge) {
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(custom_flag_palette[i]);
            auto val = blend(from, c, amt);
            bg_palette_back_buffer[16 * 12 + i] = val;
            sp_palette_back_buffer[16 + i] = val;
        }
    }

    // Tile1 palette
    if (include_tiles or not dodge) {
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(tilesheet_1_palette[i]);
            bg_palette_back_buffer[32 + i] =
                blend(from, c, include_tiles ? amt : 0);
        }
    }

    if (include_background or not dodge) {
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(background_palette[i]);
            bg_palette_back_buffer[16 * 11 + i] =
                blend(from, c, include_background ? amt : 0);
        }
    }

    // Overlay palette
    if (include_overlay or not dodge) {
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(overlay_palette[i]);
            bg_palette_back_buffer[16 + i] =
                blend(from, c, include_overlay ? amt : 0);
        }
    }
}



void Platform::Screen::pixelate(u8 amount,
                                bool include_overlay,
                                bool include_background,
                                bool include_sprites)
{
    screen_pixelate_amount = amount;

    if (amount == 0) {
        REG_MOSAIC = MOS_BUILD(0, 0, 1, 1);
    } else {
        REG_MOSAIC = MOS_BUILD(amount >> 4,
                               amount >> 4,
                               include_sprites ? amount >> 4 : 0,
                               include_sprites ? amount >> 4 : 0);

        if (include_overlay) {
            BG2_CONTROL = BG2_CONTROL | BG_MOSAIC;
        } else {
            BG2_CONTROL = BG2_CONTROL & ~BG_MOSAIC;
        }

        if (include_background) {
            BG0_CONTROL = BG0_CONTROL | BG_MOSAIC;
            BG1_CONTROL = BG1_CONTROL | BG_MOSAIC;
        } else {
            BG0_CONTROL = BG0_CONTROL & ~BG_MOSAIC;
            BG1_CONTROL = BG1_CONTROL & ~BG_MOSAIC;
        }
    }
}


static ObjectPool<PooledRcControlBlock<Platform::DynamicTexture,
                                       Platform::dynamic_texture_count>,
                  Platform::dynamic_texture_count>
    dynamic_texture_pool("dynamic-texture-pool");


void Platform::DynamicTexture::remap(u16 spritesheet_offset)
{
    auto& mapping = dynamic_texture_mappings[mapping_index_];
    mapping.dirty_ = true;
    mapping.spritesheet_offset_ = spritesheet_offset;
}


Optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    auto finalizer =
        [](PooledRcControlBlock<DynamicTexture, dynamic_texture_count>* ctrl) {
            dynamic_texture_mappings[ctrl->data_.mapping_index()].reserved_ =
                false;
            dynamic_texture_pool.free(ctrl);
        };

    for (u8 i = 0; i < dynamic_texture_count; ++i) {
        if (not dynamic_texture_mappings[i].reserved_) {
            auto dt = create_pooled_rc<DynamicTexture, dynamic_texture_count>(
                &dynamic_texture_pool, finalizer, i);
            if (dt) {
                dynamic_texture_mappings[i].reserved_ = true;
                return *dt;
            }
        }
    }

    return {};
}


void Platform::load_sprite_texture(const char* name)
{
    for (auto& mapping : dynamic_texture_mappings) {
        mapping.dirty_ = true;
    }

    for (auto& info : sprite_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_spritesheet = &info;

            init_palette(current_spritesheet->palette_data_,
                         sprite_palette,
                         ShaderPalette::spritesheet);


            // NOTE: There are four tile blocks, so index four points to the
            // end of the tile memory.
            memcpy16((void*)&MEM_TILE[4][1],
                     info.tile_data_,
                     std::min((u32)16128, info.tile_data_length_ / 2));

            // We need to do this, otherwise whatever screen fade is currently
            // active will be overwritten by the copy.
            const auto c = invoke_shader(
                real_color(last_color), ShaderPalette::spritesheet, 0);
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(sprite_palette[i]);
                MEM_PALETTE[i] = blend(from, c, last_fade_amt);
            }
        }
    }

    // By replacing the sprite texture, we overwrote whatever was in the dynamic
    // texture memory. Let's try an recover.
    map_dynamic_textures();
}



void init_darkened_palette()
{
    auto darken_c = Color::from_bgr_hex_555(tilesheet_0_palette[1]).hex();

    const auto c = invoke_shader(darken_c, ShaderPalette::tile0, 0);

    for (int i = 0; i < 16; ++i) {
        auto from = Color::from_bgr_hex_555(tilesheet_0_palette[i]);
        auto r = blend(from, c, 90);
        // MEM_BG_PALETTE[(9 * 16) + i] = r;
        tilesheet_0_darkened_palette[i] = r;
    }
}



static bool
load_img_from_file(const char* path, ShaderPalette pal, int sbb, u16* pd)
{
    auto file = filesystem::load(path, nullopt());
    if (std::get<1>(file)) {

        StringBuffer<84> name_str(path);

        if (ends_with(StringBuffer<10>(".img.bin"), name_str)) {
            StringBuffer<68> pf;
            for (char c : name_str) {
                if (c == '.') {
                    break;
                } else {
                    pf.push_back(c);
                }
            }
            pf += ".pal.bin";
            auto pal_file = filesystem::load(pf.c_str(), nullopt());
            if (not std::get<1>(pal_file)) {
                Platform::fatal(format("% missing palette", pf.c_str()));
            }
            init_palette((u16*)std::get<0>(pal_file), pd, pal);
            LZ77UnCompVram(std::get<0>(file), (void*)&MEM_SCREENBLOCKS[sbb][0]);
            return true;

        } else if (ends_with(StringBuffer<10>(".skg"), name_str)) {
            auto data = (const u16*)std::get<0>(file);
            init_palette(data, pd, pal);
            const auto c = invoke_shader(real_color(last_color), pal, 0);

            // Skip the palette section of the file...
            data += 16;
            int data_len = std::get<1>(file) - 16 * 2;

            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(pd[i]);
                MEM_BG_PALETTE[i] = blend(from, c, last_fade_amt);
            }
            memcpy16((void*)&MEM_SCREENBLOCKS[sbb][0], data, data_len);
            return true;
        }
    }

    return false;
}



void Platform::load_tile0_texture(const char* name)
{
    for (auto& info : tile_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_tilesheet0 = &info;

            init_palette(current_tilesheet0->palette_data_,
                         tilesheet_0_palette,
                         ShaderPalette::tile0);

            init_darkened_palette();

            // We don't want to load the whole palette into memory, we might
            // overwrite palettes used by someone else, e.g. the overlay...
            //
            // Also, like the sprite texture, we need to apply the currently
            // active screen fade while modifying the color palette.
            const auto c =
                invoke_shader(real_color(last_color), ShaderPalette::tile0, 0);

            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(tilesheet_0_palette[i]);
                MEM_BG_PALETTE[i] = blend(from, c, last_fade_amt);

                from = Color::from_bgr_hex_555(tilesheet_0_darkened_palette[i]);
                MEM_BG_PALETTE[(9 * 16) + i] = blend(from, c, last_fade_amt);
            }

            if (info.compressed_) {
                LZ77UnCompVram(info.tile_data_,
                               (void*)&MEM_SCREENBLOCKS[sbb_t0_texture][0]);
            } else {
                memcpy16((void*)&MEM_SCREENBLOCKS[sbb_t0_texture][0],
                         info.tile_data_,
                         std::min((int)charblock_size / 2,
                                  (int)info.tile_data_length_ / 2));
            }

            return;
        }
    }

    if (load_img_from_file(
            name, ShaderPalette::tile0, sbb_t0_texture, tilesheet_0_palette)) {
        return;
    }

    fatal(name);
}



void Platform::load_tile1_texture(const char* name)
{
    for (auto& info : tile_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_tilesheet1 = &info;

            init_palette(current_tilesheet1->palette_data_,
                         tilesheet_1_palette,
                         ShaderPalette::tile1);


            // We don't want to load the whole palette into memory, we might
            // overwrite palettes used by someone else, e.g. the overlay...
            //
            // Also, like the sprite texture, we need to apply the currently
            // active screen fade while modifying the color palette.
            const auto c =
                invoke_shader(real_color(last_color), ShaderPalette::tile1, 0);
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(tilesheet_1_palette[i]);
                MEM_BG_PALETTE[32 + i] = blend(from, c, last_fade_amt);
            }

            if (info.compressed_) {
                LZ77UnCompVram(info.tile_data_,
                               (void*)&MEM_SCREENBLOCKS[sbb_t1_texture][0]);
            } else {
                memcpy16((void*)&MEM_SCREENBLOCKS[sbb_t1_texture][0],
                         info.tile_data_,
                         std::min((int)charblock_size / 2,
                                  (int)info.tile_data_length_ / 2));
            }

            return;
        }
    }

    if (load_img_from_file(
            name, ShaderPalette::tile1, sbb_t1_texture, tilesheet_1_palette)) {
        return;
    }

    fatal(name);
}


void Platform::load_background_texture(const char* name)
{
    for (auto& info : background_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_background = &info;

            init_palette(current_background->palette_data_,
                         background_palette,
                         ShaderPalette::background);

            const auto c = invoke_shader(
                real_color(last_color), ShaderPalette::background, 0);
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(background_palette[i]);
                MEM_BG_PALETTE[16 * 11 + i] = blend(from, c, last_fade_amt);
            }

            if (validate_background_texture_size(info.tile_data_length_)) {
                if (info.compressed_) {
                    LZ77UnCompVram(
                        info.tile_data_,
                        (void*)&MEM_SCREENBLOCKS[sbb_background_texture][0]);
                } else {
                    memcpy16(
                        (void*)&MEM_SCREENBLOCKS[sbb_background_texture][0],
                        info.tile_data_,
                        (sizeof(ScreenBlock) * 2) / 2);
                }
            } else {
                StringBuffer<45> buf = "unable to load: ";
                buf += name;

                error(buf.c_str());
            }

            return;
        }
    }

    fatal(name);
}


void Platform::sleep(u32 frames)
{
    // NOTE: A sleep call should just pause the game for some number of frames,
    // but doing so should not have an impact on delta timing
    // calculation. Therefore, we need to stop the hardware timer associated
    // with the delta clock, and zero out the clock's contents when finished
    // with the sleep cycles.

    irqDisable(IRQ_TIMER3);

    // NOTE: When sleeping for large numbers of frames, we may miss button
    // presses! So we should keep track of which keys were pressed during the
    // sleep() call.
    Keyboard temp_kb;

    const auto start_keys = keyboard_.dump_state();

    while (frames--) {
        temp_kb.poll();
        const auto current_keys = temp_kb.dump_state();

        for (int i = 0; i < (int)Key::count; ++i) {
            if (start_keys[i] not_eq current_keys[i] and current_keys[i]) {
                if (not ::missed_keys) {
                    ::missed_keys.emplace();
                    ::missed_keys->clear();
                }
                missed_keys->set(i, true);
            }
        }

        watchdog_counter = 0;
        VBlankIntrWait();
    }

    irqEnable(IRQ_TIMER3);
}


bool Platform::is_running() const
{
    // Unlike the windowed desktop platform, as long as the device is
    // powered on, the game is running.
    return true;
}


static u8* const cartridge_ram = (u8*)0x0E000000;


static bool
flash_byteverify(void* in_dst, const void* in_src, unsigned int length)
{
    unsigned char* src = (unsigned char*)in_src;
    unsigned char* dst = (unsigned char*)in_dst;

    for (; length > 0; length--) {

        if (*dst++ != *src++)
            return false;
    }
    return true;
}


static void
flash_bytecpy(void* in_dst, const void* in_src, unsigned int length, bool write)
{
    unsigned char* src = (unsigned char*)in_src;
    unsigned char* dst = (unsigned char*)in_dst;

    for (; length > 0; length--) {
        if (write) {
            *(volatile u8*)0x0E005555 = 0xAA;
            *(volatile u8*)0x0E002AAA = 0x55;
            *(volatile u8*)0x0E005555 = 0xA0;
        }
        *dst++ = *src++;
    }
}


int save_capacity = 32000;


int Platform::save_capacity()
{
    return ::save_capacity;
}


static void set_flash_bank(u32 bankID)
{
    if (bankID < 2) {
        *(volatile u8*)0x0E005555 = 0xAA;
        *(volatile u8*)0x0E002AAA = 0x55;
        *(volatile u8*)0x0E005555 = 0xB0;
        *(volatile u8*)0x0E000000 = bankID;
    }
}



COLD static bool flash_save(const void* data, u32 flash_offset, u32 length)
{
    if ((u32)flash_offset >= 0x10000) {
        set_flash_bank(1);
    } else {
        set_flash_bank(0);
    }

    flash_bytecpy((void*)(cartridge_ram + flash_offset), data, length, true);

    return flash_byteverify(
        (void*)(cartridge_ram + flash_offset), data, length);
}



#define MEM_FLASH 0x0E000000
#define flash_mem ((volatile u8*)MEM_FLASH)

#define FLASH_CMD_BEGIN                                                        \
    flash_mem[0x5555] = 0xAA;                                                  \
    flash_mem[0x2AAA] = 0x55;
#define FLASH_CMD(cmd)                                                         \
    FLASH_CMD_BEGIN;                                                           \
    flash_mem[0x5555] = (cmd) << 4;



enum FlashCmd {
    FLASH_CMD_ERASE_CHIP = 1,
    FLASH_CMD_ERASE_SECTOR = 3,
    FLASH_CMD_ERASE = 8,
    FLASH_CMD_ENTER_ID_MODE = 9,
    FLASH_CMD_WRITE = 0xA,
    FLASH_CMD_SWITCH_BANK = 0xB,
    FLASH_CMD_LEAVE_ID_MODE = 0xF,
};



enum FlashManufacturer {
    FLASH_MFR_ATMEL = 0x1F,
    FLASH_MFR_PANASONIC = 0x32,
    FLASH_MFR_SANYO = 0x62,
    FLASH_MFR_SST = 0xBF,
    FLASH_MFR_MACRONIX = 0xC2,
};



enum FlashDevice {
    FLASH_DEV_MX29L010 = 0x09,
    FLASH_DEV_LE26FV10N1TS = 0x13,
    FLASH_DEV_MN63F805MNP = 0x1B,
    FLASH_DEV_MX29L512 = 0x1C,
    FLASH_DEV_AT29LV512 = 0x3D,
    FLASH_DEV_LE39FW512 = 0xD4,
};



static u32 flash_capacity()
{
    REG_WAITCNT |= WS_SRAM_8;

    FLASH_CMD(FLASH_CMD_ENTER_ID_MODE);

    busy_wait(20000);

    auto device = *(&flash_mem[1]);
    auto manufacturer = *(&flash_mem[0]);

    FLASH_CMD(FLASH_CMD_LEAVE_ID_MODE);

    busy_wait(20000);

    if (manufacturer == FLASH_MFR_SANYO) {
        flash_mem[0x5555] = FLASH_CMD_LEAVE_ID_MODE << 4;
    }

    if ((manufacturer == FLASH_MFR_MACRONIX and device == FLASH_DEV_MX29L010) or
        (manufacturer == FLASH_MFR_SANYO and
         device == FLASH_DEV_LE26FV10N1TS)) {

        info("detected 128kb flash chip. Bank switching unimplemented, using "
             "64kb.");
    }

    info("detected 64kb flash chip");
    return 64000;
}



static void flash_load(void* dest, u32 flash_offset, u32 length)
{
    if (flash_offset >= 0x10000) {
        set_flash_bank(1);
    } else {
        set_flash_bank(0);
    }

    flash_bytecpy(
        dest, (const void*)(cartridge_ram + flash_offset), length, false);
}


// NOTE: Some cartridge manufacturers back in the day searched ROMS for a
// word-aligned string, to determine what type of save memory to put on the
// chip. I designed the code to use either SRAM or FLASH, but let's include the
// backup ID string anyway, because we'd really prefer to have SRAM. Unlikely
// that anyone would ever agree to make me a GBA cartridge, but hey, you never
// know...
READ_ONLY_DATA alignas(4) [[gnu::used]] static constexpr const
    char backup_type[] = {'S', 'R', 'A', 'M', '_', 'V', 'n', 'n', 'n'};



IWRAM_CODE
void sram_save(const void* data, u32 offset, u32 length)
{
    // Supposedly, some bootleg carts require interrupts to be disabled while
    // saving, and the save code needs to run from IWRAM. Disable the sound chip
    // as well, to prevent stuttering.
    //
    // NOTE: I am highly skeptical that this code needs to be in iwram. No one
    // has ever demonstrated an example of sram writes failing with interrupts
    // enabled.
    const bool sound_was_enabled = (REG_SOUNDCNT_X & 1 << 7);
    if (sound_was_enabled) {
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xb);
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xf);
    }

    u16 ime = REG_IME;
    REG_IME = 0;

    u8* save_mem = (u8*)0x0E000000 + offset;

    // The cartridge has an 8-bit bus, so you have to write one byte at a time,
    // otherwise it won't work!
    for (size_t i = 0; i < length; ++i) {
        *save_mem++ = ((const u8*)data)[i];
    }

    REG_IME = ime;

    if (sound_was_enabled) {
        REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 9);
        REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 8);
    }
}



void sram_load(void* dest, u32 offset, u32 length)
{
    u8* save_mem = (u8*)cartridge_ram + offset;
    for (size_t i = 0; i < length; ++i) {
        ((u8*)dest)[i] = *save_mem++;
    }
}



static BootlegFlashType bootleg_flash_type = 0;



bool Platform::write_save_data(const void* data, u32 length, u32 offset)
{
    if (get_gflag(GlobalFlag::save_using_flash)) {
        return flash_save(data, offset, length);
    } else {
        sram_save(data, offset, length);

        if (bootleg_flash_type) {
            bool success =
                bootleg_flash_writeback(bootleg_flash_type, offset, length);
            if (not success) {
                info("flash write verification failed!");
                return false;
            }
        }

        return true;
    }
}


bool Platform::read_save_data(void* buffer, u32 data_length, u32 offset)
{
    if (get_gflag(GlobalFlag::save_using_flash)) {
        flash_load(buffer, offset, data_length);
    } else {
        sram_load(buffer, offset, data_length);
    }

    return true;
}



void Platform::erase_save_sector()
{
    if (not get_gflag(GlobalFlag::save_using_flash)) {
        u8* save_mem = (u8*)0x0E000000;
        // Simulate a flash erase.
        for (int i = 0; i < ::save_capacity; ++i) {
            save_mem[i] = 0xff;
        }
    } else {
        FLASH_CMD(FLASH_CMD_ERASE);
        FLASH_CMD(FLASH_CMD_ERASE_CHIP);

        info("begin flash erase!");

        // Wait for erase to complete.
        while (*((volatile u8*)0x0E000000) not_eq 0xff)
            ;
    }

    if (bootleg_flash_type) {
        info("flash erase!");
        bootleg_flash_erase(bootleg_flash_type);
    }
}



////////////////////////////////////////////////////////////////////////////////
// Logger
////////////////////////////////////////////////////////////////////////////////



Platform::Logger::Logger()
{
}



static Severity log_threshold;



void Platform::Logger::set_threshold(Severity severity)
{
    log_threshold = severity;
}



Optional<Vector<char>> log_data_;



Vector<char>* Platform::Logger::data()
{
    if (log_data_) {
        return &*log_data_;
    }

    return nullptr;
}



void Platform::Logger::log(Severity level, const char* msg)
{
    if (::__platform__ == nullptr) {
        return;
    }

    ScratchBuffer::Tag t = "syslog_data";

    if (not log_data_) {
        log_data_.emplace(t);
    }

    if (mgba_detect()) {
        mgba_log(msg);
    }

    while (*msg not_eq '\0') {
        if (*msg not_eq '\r') {
            log_data_->push_back(*msg, t);
        }
        ++msg;
    }

    log_data_->push_back('\n', t);
}



void Platform::Logger::clear()
{
    log_data_.reset();
}



void Platform::Logger::flush()
{
    if (not log_data_) {
        return;
    }

    flash_filesystem::store_file_data_binary("/log.txt", *log_data_);

    // log_data_.reset();
}



////////////////////////////////////////////////////////////////////////////////
// Speaker
//
// For music, the Speaker class uses the GameBoy's direct sound chip to play
// 8-bit signed raw audio, at 16kHz.
//
////////////////////////////////////////////////////////////////////////////////



#include "gba_platform_soundcontext.hpp"



#include "data/music_unaccompanied_wind.hpp"


static const int null_music_len = AudioBuffer::sample_count * 2;
static const u32 null_music[null_music_len] = {0};


#define DEF_AUDIO(__STR_NAME__, __TRACK_NAME__, __DIV__)                       \
    {                                                                          \
        STR(__STR_NAME__), (AudioSample*)__TRACK_NAME__,                       \
            __TRACK_NAME__##Len / __DIV__                                      \
    }


#define DEF_MUSIC(__STR_NAME__, __TRACK_NAME__)                                \
    DEF_AUDIO(__STR_NAME__, __TRACK_NAME__, 4)


#define DEF_SOUND(__STR_NAME__, __TRACK_NAME__)                                \
    DEF_AUDIO(__STR_NAME__, __TRACK_NAME__, 1)



// static const
static const struct AudioTrack
{
    const char* name_;
    const AudioSample* data_;
    int length_; // NOTE: For music, this is the track length in 32 bit words,
                 // but for sounds, length_ reprepresents bytes.
} music_tracks[] = {
    DEF_MUSIC(unaccompanied_wind, music_unaccompanied_wind),
};


static const AudioTrack* find_music(const char* name)
{
    for (auto& track : music_tracks) {

        if (str_cmp(name, track.name_) == 0) {
            return &track;
        }
    }

    return nullptr;
}


// NOTE: Between remixing the audio track down to 8-bit 16kHz signed, generating
// assembly output, adding the file to CMake, adding the include, and adding the
// sound to the sounds array, it's just too tedious to keep working this way...
#include "data/music_struttin.hpp"
#include "data/sound_archivist.hpp"
#include "data/sound_beep_error.hpp"
#include "data/sound_bell.hpp"
#include "data/sound_build0.hpp"
#include "data/sound_button_wooden.hpp"
#include "data/sound_cancel.hpp"
#include "data/sound_cannon.hpp"
#include "data/sound_click.hpp"
#include "data/sound_click_negative.hpp"
#include "data/sound_click_wooden.hpp"
#include "data/sound_cling.hpp"
#include "data/sound_coin.hpp"
#include "data/sound_creaking.hpp"
#include "data/sound_cursor_click.hpp"
#include "data/sound_digital_click_1.hpp"
#include "data/sound_drone_beep.hpp"
#include "data/sound_explosion1.hpp"
#include "data/sound_explosion2.hpp"
#include "data/sound_fizzle.hpp"
#include "data/sound_footstep1.hpp"
#include "data/sound_footstep2.hpp"
#include "data/sound_footstep3.hpp"
#include "data/sound_glass_break.hpp"
#include "data/sound_gravel.hpp"
#include "data/sound_gust.hpp"
#include "data/sound_gust2.hpp"
#include "data/sound_insert_cart.hpp"
#include "data/sound_ion_cannon.hpp"
#include "data/sound_missile.hpp"
#include "data/sound_missile_explosion.hpp"
#include "data/sound_msg.hpp"
#include "data/sound_open_book.hpp"
#include "data/sound_openbag.hpp"
#include "data/sound_page_flip.hpp"
#include "data/sound_pong_blip1.hpp"
#include "data/sound_pong_blip2.hpp"
#include "data/sound_scroll.hpp"
#include "data/sound_thunder_1.hpp"
#include "data/sound_thunder_2.hpp"
#include "data/sound_thunder_close_1.hpp"
#include "data/sound_tonal_flutter.hpp"
#include "data/sound_transporter.hpp"
#include "data/sound_tw_bell.hpp"
#include "data/sound_typewriter.hpp"
#include "data/sound_weapon_target.hpp"



static const AudioTrack sounds[] = {
    DEF_SOUND(explosion1, sound_explosion1),
    DEF_SOUND(explosion2, sound_explosion2),
    DEF_SOUND(glass_break, sound_glass_break),
    DEF_SOUND(build0, sound_build0),
    DEF_SOUND(missile, sound_missile),
    DEF_SOUND(impact, sound_missile_explosion),
    DEF_SOUND(fizzle, sound_fizzle),
    DEF_SOUND(gravel, sound_gravel),
    DEF_SOUND(beep_error, sound_beep_error),
    DEF_SOUND(drone_beep, sound_drone_beep),
    DEF_SOUND(typewriter, sound_typewriter),
    DEF_SOUND(footstep1, sound_footstep1),
    DEF_SOUND(footstep2, sound_footstep2),
    DEF_SOUND(footstep3, sound_footstep3),
    DEF_SOUND(ion_cannon, sound_ion_cannon),
    DEF_SOUND(gust1, sound_gust),
    DEF_SOUND(gust2, sound_gust2),
    DEF_SOUND(openbag, sound_openbag),
    DEF_SOUND(tw_bell, sound_tw_bell),
    DEF_SOUND(click, sound_scroll),
    DEF_SOUND(cursor_tick, sound_cursor_click),
    DEF_SOUND(click_wooden, sound_click_wooden),
    DEF_SOUND(button_wooden, sound_button_wooden),
    DEF_SOUND(click_digital_1, sound_digital_click_1),
    DEF_SOUND(cannon, sound_cannon),
    DEF_SOUND(cling, sound_cling),
    DEF_SOUND(weapon_target, sound_weapon_target),
    DEF_SOUND(transporter, sound_transporter),
    DEF_SOUND(thunder_1, sound_thunder_1),
    DEF_SOUND(thunder_2, sound_thunder_2),
    DEF_SOUND(thunder_close_1, sound_thunder_close_1),
    DEF_SOUND(pong_blip_1, sound_pong_blip1),
    DEF_SOUND(pong_blip_2, sound_pong_blip2),
    DEF_SOUND(struttin, music_struttin),
    DEF_SOUND(creaking, sound_creaking),
    DEF_SOUND(coin, sound_coin),
    DEF_SOUND(bell, sound_bell),
    DEF_SOUND(archivist, sound_archivist),
    DEF_SOUND(cancel, sound_cancel),
    DEF_SOUND(msg, sound_msg),
    DEF_SOUND(insert_cart, sound_insert_cart),
    DEF_SOUND(tonal_flutter, sound_tonal_flutter),
    DEF_SOUND(page_flip, sound_page_flip)};


static const AudioTrack* get_sound(const char* name)
{
    for (auto& sound : sounds) {
        if (str_cmp(name, sound.name_) == 0) {
            return &sound;
        }
    }
    return nullptr;
}


Microseconds Platform::Speaker::track_length(const char* name)
{
    if (const auto music = find_music(name)) {
        return (music->length_ * wordsize) / 0.016f;
    }

    if (const auto sound = get_sound(name)) {
        return sound->length_ / 0.016f;
    }

    return 0;
}


namespace detail
{
template <std::size_t... Is> struct seq
{
};
template <std::size_t N, std::size_t... Is>
struct gen_seq : gen_seq<N - 1, N - 1, Is...>
{
};
template <std::size_t... Is> struct gen_seq<0, Is...> : seq<Is...>
{
};


template <class Generator, std::size_t... Is>
constexpr auto generate_array_helper(Generator g, seq<Is...>)
    -> std::array<decltype(g(std::size_t{}, sizeof...(Is))), sizeof...(Is)>
{
    return {{g(Is, sizeof...(Is))...}};
}

template <std::size_t tcount, class Generator>
constexpr auto generate_array(Generator g)
    -> decltype(generate_array_helper(g, gen_seq<tcount>{}))
{
    return generate_array_helper(g, gen_seq<tcount>{});
}
} // namespace detail


constexpr auto make_volume_lut(float scale)
{
    return detail::generate_array<256>(
        [scale](std::size_t curr, std::size_t total) -> s8 {
            const auto real = (s8)((u8)curr);
            return real * scale;
        });
}


// Each table entry contains the whole number space of a signed 8-bit value,
// scaled by a fraction.
static constexpr std::array<VolumeScaleLUT, 20> volume_scale_LUTs = {
    {{make_volume_lut(0.05f)}, {make_volume_lut(0.10f)},
     {make_volume_lut(0.15f)}, {make_volume_lut(0.20f)},
     {make_volume_lut(0.25f)}, {make_volume_lut(0.30f)},
     {make_volume_lut(0.35f)}, {make_volume_lut(0.40f)},
     {make_volume_lut(0.45f)}, {make_volume_lut(0.50f)},
     {make_volume_lut(0.55f)}, {make_volume_lut(0.60f)},
     {make_volume_lut(0.65f)}, {make_volume_lut(0.70f)},
     {make_volume_lut(0.75f)}, {make_volume_lut(0.80f)},
     {make_volume_lut(0.85f)}, {make_volume_lut(0.90f)},
     {make_volume_lut(0.95f)}, {make_volume_lut(1.0f)}},
};



EWRAM_DATA Optional<filesystem::DirectoryCache> sounds_dir;



static Optional<ActiveSoundInfo> make_sound(const char* name)
{
    if (auto sound = get_sound(name)) {
        return ActiveSoundInfo{
            0, sound->length_, sound->data_, 0, sound->name_};
    } else {
        if (not sounds_dir) {
            sounds_dir = filesystem::find_directory("/scripts/data/sounds/");
        }
        if (not sounds_dir) {
            PLATFORM.fatal("missing /scripts/data/sounds directory "
                           "in resource bundle!");
        }
        auto f_info = filesystem::load(
            format("/scripts/data/sounds/%", name).c_str(), sounds_dir);
        if (std::get<1>(f_info)) {

            // FIXME!!!
            //
            // The game assumes that sound names are globally unique pointers to
            // null terminated strings, this used to be true with the old
            // setup. To maintain uniqueness, we use the the path string from
            // the file header...
            auto path = std::get<2>(f_info)->path_;

            auto seek_filename = [](const char* path) {
                while (*path not_eq '\0') {
                    ++path;
                }
                while (*path not_eq '/') {
                    --path;
                }
                ++path;
                return path;
            };

            path = seek_filename(path);

            return ActiveSoundInfo{.position_ = 0,
                                   .length_ = (s32)std::get<1>(f_info),
                                   .data_ =
                                       (const AudioSample*)std::get<0>(f_info),
                                   .name_ = path};
        }
        // PLATFORM.fatal(format("sound % missing!", name).c_str());
        return {};
    }
}


// FIXME: comment out of date
// If you're going to edit any of the variables used by the interrupt handler
// for audio playback, you should use this helper function.
template <typename F> auto modify_audio(F&& callback)
{
    // irqDisable(IRQ_TIMER0);
    callback();
    // irqEnable(IRQ_TIMER0);
}


bool Platform::Speaker::is_sound_playing(const char* name)
{
    bool playing = false;

    modify_audio([&] {
        for (const auto& info : snd_ctx.active_sounds) {
            if (str_eq(name, info.name_)) {
                playing = true;
                return;
            }
        }
    });

    return playing;
}



StringBuffer<48> Platform::Speaker::current_music()
{
    return snd_ctx.music_track_name;
}



bool Platform::Speaker::is_music_playing(const char* name)
{
    bool playing = false;

    if (snd_ctx.music_track_name == name) {
        return true;
    }

    if (auto track = find_music(name)) {
        modify_audio([&] {
            if (track->data_ == snd_ctx.music_track) {
                playing = true;
            }
        });
    }

    return playing;
}



Buffer<const char*, 4> completed_sounds_buffer;
volatile bool completed_sounds_lock = false;



Buffer<const char*, 4> Platform::Speaker::completed_sounds()
{
    while (completed_sounds_lock)
        ;

    Buffer<const char*, 4> result;

    completed_sounds_lock = true;

    result = completed_sounds_buffer;
    completed_sounds_buffer.clear();

    completed_sounds_lock = false;

    return result;
}



void Platform::Speaker::stop_sound(const char* name)
{
    modify_audio([&] {
        for (auto it = snd_ctx.active_sounds.begin();
             it not_eq snd_ctx.active_sounds.end();) {
            if (str_eq(name, it->name_)) {
                it = snd_ctx.active_sounds.erase(it);
            } else {
                ++it;
            }
        }
    });
}



void Platform::Speaker::clear_sounds()
{
    modify_audio([&] { snd_ctx.active_sounds.clear(); });
}


static void add_sound(Buffer<ActiveSoundInfo, 3>& sounds,
                      const ActiveSoundInfo& info)
{
    if (not sounds.full()) {
        sounds.push_back(info);
    } else {
        ActiveSoundInfo* lowest = sounds.begin();
        for (auto it = sounds.begin(); it not_eq sounds.end(); ++it) {
            if (it->priority_ < lowest->priority_) {
                lowest = it;
            }
        }

        if (lowest not_eq sounds.end() and lowest->priority_ < info.priority_) {
            sounds.erase(lowest);
            sounds.push_back(info);
        }
    }
}



static constexpr const uint __snd_rates[13] = {
    0,
    8013, // C
    7566, // C#
    7144, // D
    6742, // D#
    6362, // E
    6005, // F
    5666, // F#
    5346, // G
    5048, // G#
    4766, // A
    4499, // A#
    4246, // B
};


// 131072/(2048-n)Hz
#define SND_RATE(note, oct) (2048 - (__snd_rates[note] >> ((2 + oct))))



static constexpr const struct NoiseFrequencyTableEntry
{
    u8 shift_;
    u8 ratio_;
} noise_frequency_table_[57] = {
    {0, 0},  {1, 0},  {2, 0},  {0, 3},  {3, 0},  {0, 5},  {1, 3},  {0, 7},
    {4, 0},  {1, 5},  {2, 3},  {1, 7},  {5, 0},  {2, 5},  {3, 3},  {2, 7},
    {6, 0},  {3, 5},  {4, 3},  {3, 7},  {7, 0},  {4, 5},  {5, 3},  {4, 7},
    {8, 0},  {5, 5},  {6, 3},  {5, 7},  {9, 0},  {6, 5},  {7, 3},  {6, 7},
    {10, 0}, {7, 5},  {8, 3},  {7, 7},  {11, 0}, {8, 5},  {9, 3},  {8, 7},
    {12, 0}, {9, 5},  {10, 3}, {9, 7},  {13, 0}, {10, 5}, {11, 3}, {10, 7},
    {11, 5}, {12, 3}, {11, 7}, {12, 5}, {13, 3}, {12, 7}, {13, 5}, {13, 7}};



struct AnalogChannel
{
    Platform::Speaker::Note last_note_;
    u8 last_octave_;
    Microseconds effect_timer_;
};



static EWRAM_DATA AnalogChannel analog_channel[4];



void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   Optional<Vec2<Float>> position)
{
    (void)position; // We're not using position data, because on the gameboy
                    // advance, we aren't supporting spatial audio.

    if (auto info = make_sound(name)) {
        info->priority_ = priority;

        modify_audio([&] { add_sound(snd_ctx.active_sounds, *info); });
    }
}


static void clear_music()
{
    // The audio interrupt handler can be smaller and simpler if we use a track
    // of empty bytes to represent scenarios where music is not playing, rather
    // than adding another if condition to the audio isr.
    snd_ctx.music_track = reinterpret_cast<const AudioSample*>(null_music);
    snd_ctx.music_track_length = null_music_len - 1;
    snd_ctx.music_track_name = "(null)";
    snd_ctx.music_track_pos = 0;
}


static void stop_music()
{
    modify_audio([] { clear_music(); });
}



void Platform::Speaker::stop_music()
{
    ::stop_music();
}



bool Platform::Speaker::stream_music(const char* filename, Microseconds offset)
{
    play_music(filename, offset); // In case it's a builtin track
    if (is_music_playing(filename)) {
        return true;
    }

    auto found = PLATFORM.load_file("scripts/data/music", filename);

    if (not found.second) {
        found = PLATFORM.load_file("", filename);
        if (not found.second) {
            return false;
        }
    }

    const Microseconds sample_offset = offset * 0.016f; // NOTE: because 16kHz

    modify_audio([&] {
        static const int wordsize = 4;

        snd_ctx.music_track_length = found.second / wordsize - 4;
        snd_ctx.music_track = (AudioSample*)found.first;

        snd_ctx.music_track_pos =
            (sample_offset / wordsize) % found.second / wordsize;
        snd_ctx.music_track_name = filename;
    });

    return true;
}



static void play_music(const char* name, Microseconds offset)
{
    const auto track = find_music(name);
    if (track == nullptr) {
        return;
    }

    const Microseconds sample_offset = offset * 0.016f; // NOTE: because 16kHz

    modify_audio([&] {
        snd_ctx.music_track_length = track->length_;
        snd_ctx.music_track = track->data_;
        snd_ctx.music_track_pos = (sample_offset / 4) % track->length_;
        snd_ctx.music_track_name = name;
    });
}



static void audio_update_halfspeed_isr();
void Platform::Speaker::play_music(const char* name, Microseconds offset)
{
    // NOTE: The sound sample needs to be mono, and 8-bit signed. To export this
    // format from Audacity, convert the tracks to mono via the Tracks dropdown,
    // and then export as raw, in the format 8-bit signed.
    //
    // Also, important to convert the sound file frequency to 16kHz.

    this->stop_music();

    ::play_music(name, offset);

    // FIXME!!!!!! Mysteriously, there's a weird audio glitch, where the sound
    // effects, but not the music, get all glitched out until two sounds are
    // played consecutively. I've spent hours trying to figure out what's going
    // wrong, and I haven't solved this one yet, so for now, just play a couple
    // quiet sounds. To add further confusion, after adjusting the instruction
    // prefetch and waitstats, I need to play three sounds
    // consecutively... obviously my interrupt service routine for the audio is
    // flawed somehow. Do I need to completely disable the timers and sound
    // chip, as well as the audio interrupts, when playing new sounds? Does
    // disabling the audio interrupts when queueing a new sound effect cause
    // audio artifacts, because the sound chip is not receiving samples?
    play_sound("footstep1", 0);
    play_sound("footstep2", 0);
    play_sound("footstep3", 0);

    // auto tmp = allocate_dynamic<OptDmaBufferData>("test");
    // auto before = __platform__->delta_clock().sample();
    // // audio_update_fast_isr();

    // alignas(4) u16 tmp2[162];

    // win_circle(tmp->data(),
    //            0,
    //            0,
    //            156);

    // auto after = __platform__->delta_clock().sample();
    // Platform::fatal(format("dt %", after - before));
}


Platform::Speaker::Speaker()
{
}


////////////////////////////////////////////////////////////////////////////////
// Misc
////////////////////////////////////////////////////////////////////////////////



void Platform::on_unrecoverrable_error(UnrecoverrableErrorCallback callback)
{
    ::unrecoverrable_error_callback.emplace(callback);
}



std::pair<const char*, u32> Platform::load_file(const char* folder,
                                                const char* filename) const
{
    StringBuffer<64> path("/");

    if (strlen(folder) > 0) {
        path += folder;
        path += "/";
    } else if (*filename == '/') {
        // Redundant beginning slash.
        path.clear();
    }

    path += filename;

    auto info = filesystem::load(path.c_str(), nullopt());
    return {std::get<0>(info), std::get<1>(info)};
}



void Platform::walk_filesystem(
    Function<8 * sizeof(void*), void(const char* path)> callback)
{
    filesystem::walk(callback);
}



Platform::~Platform()
{
    // ...
}



struct GlyphMapping
{
    u16 mapper_offset_;

    // -1 represents unassigned. Mapping a tile into memory sets the reference
    //  count to zero. When a call to Platform::set_tile reduces the reference
    //  count back to zero, the tile is once again considered to be unassigned,
    //  and will be set to -1.
    s16 reference_count_ = 0;
    bool unused_ = true;
};


static constexpr const auto glyph_start_offset = 1;
static constexpr const auto glyph_mapping_count = 78;


static constexpr const auto glyph_expanded_count = 160;


static int glyph_table_size = glyph_mapping_count;


static const int font_color_index_tile = 81;


struct GlyphTable
{
    GlyphMapping mappings_[glyph_mapping_count + glyph_expanded_count];
};

static EWRAM_DATA GlyphTable glyph_table;



int gc_glyphs()
{
    int collected = 0;

    for (auto& glyph : glyph_table.mappings_) {
        if (glyph.reference_count_ <= 0) {
            glyph.unused_ = true;
            glyph.reference_count_ = 0;
            ++collected;
        }
    }

    return collected;
}



static const VolumeScaleLUT* music_volume_lut = &volume_scale_LUTs[19];
static const VolumeScaleLUT* sound_volume_lut = &volume_scale_LUTs[19];


struct SoundMixerCallback
{
    void (*isr_)();
    int output_words_per_callback_;
};


#define SOUND_MIXER_CALLBACK(NAME, RATE)                                       \
    static const SoundMixerCallback NAME##_cb                                  \
    {                                                                          \
        NAME##_isr, RATE                                                       \
    }


SOUND_MIXER_CALLBACK(audio_update_fast, 2);


static void audio_update_music_volume_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    // NOTE: audio tracks in ROM should therefore have four byte alignment!
    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos++];

    if (UNLIKELY(snd_ctx.music_track_pos > snd_ctx.music_track_length)) {
        snd_ctx.music_track_pos = 0;
    }

    for (AudioSample& s : mixing_buffer) {
        s = (*music_volume_lut)[s];
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {

        auto pos = it->position_;
        it->position_ += 4;

        if (UNLIKELY(it->position_ >= it->length_)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            auto buf = mixing_buffer;
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos++]];
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos++]];
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos++]];
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos]];
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_music_volume, 1);



static void audio_update_slow_rewind_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos];

    std::swap(mixing_buffer[0], mixing_buffer[3]);
    std::swap(mixing_buffer[1], mixing_buffer[2]);

    snd_ctx.music_track_pos -= 1;
    if (snd_ctx.music_track_pos < 1) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 4 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (u8)it->data_[it->position_];
                it->position_ -= 1;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_slow_rewind_music, 1);



static void audio_update_rewind_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];
    alignas(4) AudioSample mixing_buffer2[4];

    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 2];

    *((u32*)mixing_buffer2) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 1];

    // NOTE: we're dropping half of the samples and putting the other half into
    // the ouptut buffer in reverse order, to rewind the music at 2x speed. But
    // of course, we can't just drop the first half of the samples and keep the
    // second half, we drop every other note. mixing_buffer[2] happens to be in
    // the correct position already.
    mixing_buffer[3] = mixing_buffer[0];
    mixing_buffer[0] = mixing_buffer2[2];
    mixing_buffer[1] = mixing_buffer2[0];

    snd_ctx.music_track_pos -= 2;
    if (snd_ctx.music_track_pos < 2) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 8 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (u8)it->data_[it->position_];
                it->position_ -= 2;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_rewind_music, 1);



static void audio_update_rewind4x_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    // Four-times rewind speed. Pick the first byte of the prior four words.
    mixing_buffer[0] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);
    mixing_buffer[1] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);
    mixing_buffer[2] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);
    mixing_buffer[3] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);

    if (snd_ctx.music_track_pos < 4) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 16 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (s8)it->data_[it->position_];
                it->position_ -= 4;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_rewind4x_music, 1);



static void audio_update_rewind8x_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    // Four-times rewind speed. Pick the first byte of the prior four words.
    mixing_buffer[0] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;
    mixing_buffer[1] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;
    mixing_buffer[2] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;
    mixing_buffer[3] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;

    if (snd_ctx.music_track_pos < 8) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 32 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (s8)it->data_[it->position_];
                it->position_ -= 8;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_rewind8x_music, 1);



static void audio_update_doublespeed_isr()
{
    alignas(4) AudioSample mixing_buffer[4];
    alignas(4) AudioSample mixing_buffer2[4];

    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 2];

    *((u32*)mixing_buffer2) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 1];

    mixing_buffer[1] = mixing_buffer[2];
    mixing_buffer[2] = mixing_buffer2[0];
    mixing_buffer[3] = mixing_buffer2[2];


    snd_ctx.music_track_pos += 2;

    if (UNLIKELY(snd_ctx.music_track_pos > snd_ctx.music_track_length + 2)) {
        snd_ctx.music_track_pos = 0;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ + 8 >= it->length_)) {
            if (not completed_sounds_lock) {
                completed_sounds_buffer.push_back(it->name_);
            }
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[i] += (u8)it->data_[it->position_];
                it->position_ += 2;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_doublespeed, 1);



static void audio_update_halfspeed_isr()
{
    // NOTE: rather than change the logic for indices into the music track, I
    // just exploit the unused depth of the sound fifo and run the isr half the
    // time.
    alignas(4) AudioSample mixing_buffer[4];

    // NOTE: audio tracks in ROM should therefore have four byte alignment!
    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos++];

    if (UNLIKELY(snd_ctx.music_track_pos > snd_ctx.music_track_length)) {
        snd_ctx.music_track_pos = 0;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ + 4 >= it->length_)) {
            if (not completed_sounds_lock) {
                completed_sounds_buffer.push_back(it->name_);
            }
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[i] += (u8)it->data_[it->position_];
                ++it->position_;
            }
            ++it;
        }
    }

    alignas(4) AudioSample mixing_buffer_out_1[4];
    alignas(4) AudioSample mixing_buffer_out_2[4];

    mixing_buffer_out_1[0] = mixing_buffer[0];
    mixing_buffer_out_1[1] = mixing_buffer[0];

    mixing_buffer_out_1[2] = mixing_buffer[1];
    mixing_buffer_out_1[3] = mixing_buffer[1];

    mixing_buffer_out_2[0] = mixing_buffer[2];
    mixing_buffer_out_2[1] = mixing_buffer[2];

    mixing_buffer_out_2[2] = mixing_buffer[3];
    mixing_buffer_out_2[3] = mixing_buffer[3];

    REG_SGFIFOA = *((u32*)mixing_buffer_out_1);
    REG_SGFIFOA = *((u32*)mixing_buffer_out_2);
}
SOUND_MIXER_CALLBACK(audio_update_halfspeed, 2);



EWRAM_DATA static volatile bool audio_update_swapflag;
EWRAM_DATA static void (*audio_update_current_isr)() = audio_update_fast_isr;
EWRAM_DATA static u8 audio_update_current_freq;
EWRAM_DATA static u8 audio_update_new_freq;



static void audio_update_swap_isr()
{
    audio_update_current_isr();

    // If the new isr fires less often than the replacement, fill the audio fifo
    // accordingly.
    const s8 gap_fill = (audio_update_new_freq - audio_update_current_freq) - 1;
    for (s8 i = 0; i < gap_fill; ++i) {
        audio_update_current_isr();
    }

    audio_update_swapflag = true;
}



static void audio_update_swap(const SoundMixerCallback& cb)
{
    audio_update_swapflag = false;

    audio_update_new_freq = cb.output_words_per_callback_;

    irqSet(IRQ_TIMER1, audio_update_swap_isr);

    // We have to poll on a flag, because we want to change the timer frequency
    // in some cases. We can't write a new value to the timer configuration
    // register until just after it's fired, or sound stuff could get out of
    // sync.
    while (not audio_update_swapflag)
        ;

    audio_update_current_freq = audio_update_new_freq;

    REG_TM1CNT_L = audio_timer_frequency(cb.output_words_per_callback_);
    irqSet(IRQ_TIMER1, cb.isr_);
    audio_update_current_isr = cb.isr_;
}



void Platform::Speaker::set_music_speed(MusicSpeed speed)
{
    if (__platform__->network_peer().is_connected()) {
        audio_update_swap(audio_update_fast_cb);
        return;
    }

    switch (speed) {
    default:
    case MusicSpeed::regular:
        audio_update_swap(audio_update_fast_cb);
        break;

    case MusicSpeed::doubled:
        audio_update_swap(audio_update_doublespeed_cb);
        break;

    case MusicSpeed::reversed:
        audio_update_swap(audio_update_rewind_music_cb);
        break;

    case MusicSpeed::reversed4x:
        audio_update_swap(audio_update_rewind4x_music_cb);
        break;

    case MusicSpeed::reversed8x:
        audio_update_swap(audio_update_rewind8x_music_cb);
        break;

    case MusicSpeed::reversed_slow:
        audio_update_swap(audio_update_slow_rewind_music_cb);
        break;

    case MusicSpeed::halved:
        audio_update_swap(audio_update_halfspeed_cb);
        break;
    }
}



void Platform::Speaker::set_music_volume(u8 volume)
{
    if (volume >= volume_scale_LUTs.size()) {
        return;
    }

    if (volume == volume_scale_LUTs.size() - 1) {
        // Ok, so... I'm aware that the platform header would appear to support
        // changing sound effect volume without modifying music volume. Maybe
        // I'll fix it sometime... but for now, sound volume may only be
        // adjusted when music volume is not at maximum. I just never had a
        // reason to fade sounds without fading music as well.
        //
        // Maybe, I'll fix it sometime. But my future plans for this project,
        // after the gba release, just involve targetting other platforms, so
        // eventually, this defficiency won't matter anymore when this file
        // becomes legacy code.
        //
        // Modifying sound/music volume in a timer irq is a heavy operation, so
        // I have no real incentive to make the sound volume behave as expected
        // unless I actually need the behavior for implementing features.
        audio_update_swap(audio_update_fast_cb);
    } else {
        music_volume_lut = &volume_scale_LUTs[volume];
        audio_update_swap(audio_update_music_volume_cb);
    }
}



void Platform::Speaker::set_sounds_volume(u8 volume)
{
    if (volume >= volume_scale_LUTs.size()) {
        return;
    }

    sound_volume_lut = &volume_scale_LUTs[volume];
}



static Buffer<ActiveSoundInfo, 3> sound_stash;



void Platform::Speaker::stash_sounds()
{
    sound_stash.clear();

    modify_audio([&] {
        sound_stash = snd_ctx.active_sounds;
        snd_ctx.active_sounds.clear();
    });
}



void Platform::Speaker::restore_sounds()
{
    modify_audio([&] { snd_ctx.active_sounds = sound_stash; });
}



static void audio_start()
{
    clear_music();

    // REG_SOUNDCNT_H =
    //     0x0B0D | SDS_DMG100; //DirectSound A + fifo reset + max volume to L and R
    REG_SOUNDCNT_X = 0x0080; //turn sound chip on

    REG_SOUNDCNT_H = SDS_DMG100 | 1 << 2 | 1 << 3 | 1 << 8 | 1 << 9;


    // Required for stereo, currently unused.
    // // Both direct sound channels, FIFO reset, A is R, B is L.
    // REG_SOUNDCNT_H = 0b1010100100001111;
    // REG_SOUNDCNT_X = 0x0080; //turn sound chip on

    audio_update_fast_isr();


    irqEnable(IRQ_TIMER1);
    irqSet(IRQ_TIMER1, audio_update_fast_isr);
    audio_update_current_freq = audio_update_fast_cb.output_words_per_callback_;

    REG_TM0CNT_L = 0xffff;
    REG_TM1CNT_L = audio_timer_frequency(audio_update_current_freq);

    // While it may look like TM0 is unused, it is in fact used for setting the
    // sample rate for the digital audio chip.
    REG_TM0CNT_H = 0x0083;
    REG_TM1CNT_H = 0x00C3;


    // turn sound on
    REG_SNDSTAT = SSTAT_ENABLE;

    // on left/right ; both full volume
    REG_SNDDMGCNT =
        SDMG_BUILD_LR(SDMG_SQR1 | SDMG_SQR2 | SDMG_WAVE | SDMG_NOISE, 7);

    // no sweep
    REG_SND1SWEEP = SSW_OFF;

    // envelope: vol=12, decay, max step time (7) ; 50% duty
    REG_SND1CNT = SSQR_ENV_BUILD(12, 0, 7) | SSQR_DUTY1_2;
    REG_SND1FREQ = 0;

    REG_SND2CNT = SSQR_ENV_BUILD(12, 0, 7) | SSQR_DUTY1_4;
    REG_SND2FREQ = 0;

    REG_SND4CNT = SSQR_ENV_BUILD(12, 0, 7) | SSQR_DUTY1_4;
    REG_SND4FREQ = 0;
}



static void remote_console_start();



namespace
{
__attribute__((section(".ewram"))) int _ewram_static_data = 0;
}


bool ram_overclock()
{
    const u16 prev_dispcnt = REG_DISPCNT;

    volatile unsigned& memctrl_register =
        *reinterpret_cast<unsigned*>(0x4000800);
    memctrl_register = 0x0E000020;

    volatile int& ewram_static_data = _ewram_static_data;
    ewram_static_data = 1;

    bool result = false;

    if (ewram_static_data != 1) {
        memctrl_register = 0x0D000020;
        info("ewram overclocking disabled");
        result = false;
    } else {
        info("overclocked ewram");
        result = true;
    }

    // In MyBoy! and a few other inaccurate emulators, ewram overclocking
    // overwrites REG_DISPCNT for some reason.
    const bool emulator_bug = REG_DISPCNT not_eq prev_dispcnt;
    REG_DISPCNT = prev_dispcnt;

    if (emulator_bug) {
        info("The software has detected that the emulator that you're running "
             "this thing on kinda sucks.");
    }

    return result;
}



void show_health_and_safety_message()
{
    // Throwaway platform-specific code for displaying a health an safetly
    // warning, like late-era gba games.

    Color c_white(custom_color(0xffffff));
    static const auto white_555 = c_white.bgr_hex_555();

    u16 cached_palette[16];

    VBlankIntrWait();

    for (u32 i = 0; i < 16; ++i) {
        MEM_BG_PALETTE[i] = white_555;
        MEM_BG_PALETTE[i + 16] = white_555;
    }

    for (auto& info : tile_textures) {
        if (str_eq(info.name_, "gba_health_safety_logo_flattened")) {

            memcpy(cached_palette, info.palette_data_, 32);

            if (validate_tilemap_texture_size(info.tile_data_length_)) {
                VBlankIntrWait();
                memcpy16((void*)&MEM_SCREENBLOCKS[sbb_t0_texture][0],
                         info.tile_data_,
                         info.tile_data_length_ / 2);
            } else {
                error("Unable to load health and safety notice.");
            }

            break;
        }
    }

    REG_DISPCNT = MODE_0 | OBJ_ENABLE | OBJ_MAP_1D | BG0_ENABLE;
    BG0_X_SCROLL = 0;
    BG0_Y_SCROLL = 0;

    BG0_CONTROL = BG_CBB(cbb_t0_texture) | BG_SBB(sbb_t0_tiles) | BG_REG_64x32 |
                  BG_PRIORITY(2) | BG_MOSAIC;

    auto set_tile = [&](int x, int y, int val, int palette) {
        MEM_SCREENBLOCKS[sbb_t0_tiles][x + y * 32] = val | SE_PALBANK(palette);
    };

    u16 tile = 1;
    for (u16 y = 0; y < 11; ++y) {
        for (u16 x = 0; x < 30; ++x) {
            set_tile(x, y, tile++, 0);
        }
    }

    for (int x = 0; x < 30; ++x) {
        set_tile(x, 11, 1, 0);
    }

    for (u16 y = 12; y < 16; ++y) {
        for (u16 x = 0; x < 30; ++x) {
            set_tile(x, y, tile++, 0);
        }
    }

    for (int x = 0; x < 30; ++x) {
        set_tile(x, 16, 1, 0);
    }

    for (u16 y = 17; y < 19; ++y) {
        for (u16 x = 0; x < 30; ++x) {
            set_tile(x, y, tile++, 1);
        }
    }

    for (int x = 0; x < 30; ++x) {
        set_tile(x, 19, 1, 0);
    }

    for (int i = 0; i < 5; ++i) {
        VBlankIntrWait();
    }

    for (int i = 0; i < 20; ++i) {
        auto blend_amount = 255 * ((float)i / 20);
        VBlankIntrWait();
        for (int i = 0; i < palette_count; ++i) {
            const auto c = Color::from_bgr_hex_555(cached_palette[i]);
            MEM_BG_PALETTE[i] = blend(c_white, c, blend_amount);
        }
    }

    int frames = 0;
    u8 hint_fade_amount = 0;
    bool hint_fade_in = true;

    while (true) {
        ++frames;
        PLATFORM_EXTENSION(feed_watchdog);
        VBlankIntrWait();

        if (frames > 60) {
            if (hint_fade_in) {
                hint_fade_amount += 15;
                if (hint_fade_amount == 255) {
                    hint_fade_in = false;
                }
            } else {
                hint_fade_amount -= 15;
                if (hint_fade_amount == 0) {
                    hint_fade_in = true;
                }
            }
            VBlankIntrWait();
            for (int i = 0; i < palette_count; ++i) {
                const auto c = Color::from_bgr_hex_555(cached_palette[i]);
                MEM_BG_PALETTE[i + 16] = blend(c_white, c, hint_fade_amount);
            }
        }

        missed_keys.reset();
        PLATFORM.keyboard().poll();
        bool exit = false;
        for (int i = 0; i < (int)Key::count; ++i) {
            if (PLATFORM.keyboard().pressed((Key)i)) {
                exit = true;
                break;
            }
        }
        if (exit) {
            break;
        }
    }

    while (hint_fade_amount) {
        hint_fade_amount -= 15;
        VBlankIntrWait();
        for (int i = 0; i < palette_count; ++i) {
            const auto c = Color::from_bgr_hex_555(cached_palette[i]);
            MEM_BG_PALETTE[i + 16] = blend(c_white, c, hint_fade_amount);
        }
    }

    for (int i = 0; i < 20; ++i) {
        auto blend_amount = 255 * ((float)i / 20);
        VBlankIntrWait();
        for (int i = 0; i < palette_count; ++i) {
            const auto c = Color::from_bgr_hex_555(cached_palette[i]);
            MEM_BG_PALETTE[i] = blend(c, c_white, blend_amount);
        }
    }

    for (int i = 0; i < 10; ++i) {
        VBlankIntrWait();
    }
}



void Platform::enable_glyph_mode(bool enabled)
{
    if (enabled) {
        for (auto& gm : ::glyph_table.mappings_) {
            gm.reference_count_ = 0;
            gm.unused_ = true;
        }
    }
    set_gflag(GlobalFlag::glyph_mode, enabled);
}



u8* overlay_vram_tile_data(u16 tile_index)
{
    return (u8*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0] +
           ((tile_index)*vram_tile_size());
}



bool Platform::load_overlay_texture(const char* name)
{
    for (auto& info : overlay_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_overlay_texture = &info;

            init_palette(current_overlay_texture->palette_data_,
                         overlay_palette,
                         ShaderPalette::overlay);

            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(overlay_palette[i]);
                if (not overlay_was_faded) {
                    MEM_BG_PALETTE[16 + i] = from.bgr_hex_555();
                } else {
                    const auto c = invoke_shader(
                        real_color(last_color), ShaderPalette::overlay, i);
                    MEM_BG_PALETTE[16 + i] = blend(from, c, last_fade_amt);
                }
            }

            constexpr auto charblock_size = sizeof(ScreenBlock) * 8;


            if (info.compressed_) {
                LZ77UnCompVram(
                    info.tile_data_,
                    (void*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0]);

            } else {
                memcpy16((void*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0],
                         info.tile_data_,
                         std::min((size_t)info.tile_data_length_ / 2,
                                  charblock_size / 2));
            }

            if (get_gflag(GlobalFlag::glyph_mode)) {
                for (auto& gm : ::glyph_table.mappings_) {
                    gm.reference_count_ = 0;
                    gm.unused_ = true;
                }
            }

            return true;
        }
    }

    if (load_img_from_file(name,
                           ShaderPalette::overlay,
                           sbb_overlay_texture,
                           overlay_palette)) {
        return true;
    }

    return false;
}


static const TileDesc bad_glyph = 495;


// Rather than doing tons of extra work to keep the palettes
// coordinated between different image files, use tile index
// 81 as a registration block, which holds a set of colors
// to use when mapping glyphs into vram.
static u8* font_index_tile()
{
    return (u8*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0] +
           ((font_color_index_tile)*vram_tile_size());
}


struct FontColorIndices
{
    int fg_;
    int bg_;
};


static FontColorIndices font_color_indices()
{
    const auto index = font_index_tile();
    return {index[0] & 0x0f, index[1] & 0x0f};
}


// This code uses a lot of naive algorithms for searching the glyph mapping
// table, potentially could be sped up, but we don't want to use any extra
// memory, we've only got 256K to work with, and the table is big enough as it
// is.
TileDesc Platform::map_glyph(const utf8::Codepoint& glyph,
                             const TextureMapping& mapping_info)
{
    if (not get_gflag(GlobalFlag::glyph_mode)) {
        return bad_glyph;
    }

    // const auto mapping_info = mapper(glyph);

    // if (not mapping_info) {
    //     return bad_glyph;
    // }

    for (TileDesc tile = 0; tile < glyph_table_size; ++tile) {
        auto& gm = ::glyph_table.mappings_[tile];
        if (not gm.unused_ and gm.mapper_offset_ == mapping_info.offset_) {
            return glyph_start_offset + tile;
        }
    }

    for (auto& info : overlay_textures) {
        if (str_cmp(mapping_info.texture_name_, info.name_) == 0) {

            bool retried = false;

        RETRY:
            for (TileDesc t = 0; t < glyph_table_size; ++t) {

                if (t == font_color_index_tile - 1) {
                    // When I originally created the text mapping engine, I did
                    // not expect to need to deal with languages with more than
                    // 80 distinct font tiles onscreen at a time. So, I thought
                    // it would be fine to put a metadata tile in index 81. But
                    // while working on the Chinese localization, I discovered
                    // that 80 tiles would not be nearly sufficient to display a
                    // fullscreen block of chinese words. So I needed to build a
                    // dynamically-expandable glyph table, which, when needed,
                    // can expand to consume more of the available vram. So, we
                    // need to skip over this metadata tile, to make sure that
                    // we don't overwrite it when using a larger glyph array.
                    continue;
                }

                auto& gm = ::glyph_table.mappings_[t];
                if (gm.unused_) {
                    gm.mapper_offset_ = mapping_info.offset_;
                    gm.reference_count_ = 0;
                    gm.unused_ = false;
                    // 8 x 8 x (4 bitsperpixel / 8 bitsperbyte)
                    constexpr int tile_size = vram_tile_size();

                    // u8 buffer[tile_size] = {0};
                    // memcpy16(buffer,
                    //          (u8*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0] +
                    //              ((81) * tile_size),
                    //          tile_size / 2);

                    const auto colors = font_color_indices();

                    // We need to know which color to use as the background
                    // color, and which color to use as the foreground
                    // color. Each charset needs to store a reference pixel in
                    // the top left corner, representing the background color,
                    // otherwise, we have no way of knowing which pixel color to
                    // substitute where!
                    const auto bg_color = ((u8*)info.tile_data_)[0] & 0x0f;

                    alignas(u32) u8 buffer[tile_size] = {0};
                    memcpy32(buffer,
                             info.tile_data_ +
                                 ((u32)mapping_info.offset_ * tile_size) /
                                     sizeof(decltype(info.tile_data_)),
                             tile_size / 4);

                    for (int i = 0; i < tile_size; ++i) {
                        auto c = buffer[i];
                        if (c & bg_color) {
                            buffer[i] = colors.bg_;
                        } else {
                            buffer[i] = colors.fg_;
                        }
                        if (c & (bg_color << 4)) {
                            buffer[i] |= colors.bg_ << 4;
                        } else {
                            buffer[i] |= colors.fg_ << 4;
                        }
                    }

                    // FIXME: Why do these magic constants work? I wish better
                    // documentation existed for how the gba tile memory worked.
                    // I thought, that the tile size would be 32, because we
                    // have 4 bits per pixel, and 8x8 pixel tiles. But the
                    // actual number of bytes in a tile seems to be half of the
                    // expected number. Also, in vram, it seems like the tiles
                    // do seem to be 32 bytes apart after all...
                    memcpy16((u8*)&MEM_SCREENBLOCKS[sbb_overlay_texture][0] +
                                 ((t + glyph_start_offset) * tile_size),
                             buffer,
                             tile_size / 2);

                    return t + glyph_start_offset;
                }
            }

            if (not retried) {
                retried = true;
                gc_glyphs();
                goto RETRY;
            }
        }
    }
    return bad_glyph;
}


static bool is_glyph(u16 t)
{
    return t >= glyph_start_offset and
           t - glyph_start_offset < glyph_table_size;
}


void Platform::fill_overlay(u16 tile)
{
    if (get_gflag(GlobalFlag::glyph_mode) and is_glyph(tile)) {
        // This is moderately complicated to implement, better off just not
        // allowing fills for character tiles.
        return;
    }

    const u16 tile_info = tile | SE_PALBANK(1);
    const u32 fill_word = tile_info | (tile_info << 16);

    u32* const mem = (u32*)overlay_back_buffer;
    overlay_back_buffer_changed = true;

    for (unsigned i = 0; i < sizeof(ScreenBlock) / wordsize; ++i) {
        mem[i] = fill_word;
    }

    if (get_gflag(GlobalFlag::glyph_mode)) {
        for (auto& gm : ::glyph_table.mappings_) {
            gm.reference_count_ = 0;
            gm.unused_ = true;
        }
    }
}



static void set_overlay_tile(u16 x, u16 y, u16 val, int palette)
{
    if (get_gflag(GlobalFlag::glyph_mode)) {
        // This is where we handle the reference count for mapped glyphs. If
        // we are overwriting a glyph with different tile, then we can
        // decrement a glyph's reference count. Then, we want to increment
        // the incoming glyph's reference count if the incoming tile is
        // within the range of the glyph table.

        const auto old_tile = PLATFORM.get_tile(Layer::overlay, x, y);
        if (old_tile not_eq val) {
            if (is_glyph(old_tile)) {
                auto& gm =
                    ::glyph_table.mappings_[old_tile - glyph_start_offset];
                if (not gm.unused_) {
                    gm.reference_count_ -= 1;

                    if (gm.reference_count_ < 0) {
                        gm.reference_count_ = 0;
                    }
                } else {
                    // I suppose this could happen if we swapped overlay
                    // textures, without clearing out existing tiles? Not
                    // sure. The code has been stable in the real world for
                    // quite a while, so I'm commenting out the log line.
                    //
                    // error(pfrm,
                    //       "existing tile is a glyph, but has no"
                    //       " mapping table entry!");
                }
            }

            if (is_glyph(val)) {
                auto& gm = ::glyph_table.mappings_[val - glyph_start_offset];
                if (gm.unused_) {
                    Platform::fatal("invalid assignment to glyph table");
                }
                gm.reference_count_++;
            }
        }
    }

    overlay_back_buffer[x + y * 32] = val | SE_PALBANK(palette);
    overlay_back_buffer_changed = true;
}


// Now for custom-colored text... we're using three of the background palettes
// already, one for the map_0 layer (shared with the background layer), one for
// the map_1 layer, and one for the overlay. That leaves 13 remaining palettes,
// which we can use for colored text. But we may not want to use up all of the
// available extra palettes, so let's just allocate four of them toward custom
// text colors for now...
static const PaletteBank custom_text_palette_begin = 3;
static const PaletteBank custom_text_palette_end = 9;
static const auto custom_text_palette_count =
    custom_text_palette_end - custom_text_palette_begin;

static PaletteBank custom_text_palette_write_ptr = custom_text_palette_begin;
static const TextureData* custom_text_palette_source_texture = nullptr;



void Platform::set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors)
{
    if (not get_gflag(GlobalFlag::glyph_mode) or not is_glyph(glyph)) {
        return;
    }

    // If the current overlay texture changed, then we'll need to clear out any
    // palettes that we've constructed. The indices of the glyph binding sites
    // in the palette bank may have changed when we loaded a new texture.
    if (custom_text_palette_source_texture and
        custom_text_palette_source_texture not_eq current_overlay_texture) {

        for (auto p = custom_text_palette_begin; p < custom_text_palette_end;
             ++p) {
            for (int i = 0; i < 16; ++i) {
                MEM_BG_PALETTE[p * 16 + i] = 0;
            }
        }

        custom_text_palette_source_texture = current_overlay_texture;
    }

    const auto default_colors = font_color_indices();

    const auto fg_color_hash =
        invoke_shader(real_color(colors.foreground_), ShaderPalette::overlay, 0)
            .bgr_hex_555();

    const auto bg_color_hash =
        invoke_shader(real_color(colors.background_), ShaderPalette::overlay, 0)
            .bgr_hex_555();

    auto existing_mapping = [&]() -> Optional<PaletteBank> {
        for (auto i = custom_text_palette_begin; i < custom_text_palette_end;
             ++i) {
            if (MEM_BG_PALETTE[i * 16 + default_colors.fg_] == fg_color_hash and
                MEM_BG_PALETTE[i * 16 + default_colors.bg_] == bg_color_hash) {

                return i;
            }
        }
        return {};
    }();

    if (existing_mapping) {
        set_overlay_tile(x, y, glyph, *existing_mapping);
    } else {
        const auto target = custom_text_palette_write_ptr;

        MEM_BG_PALETTE[target * 16 + default_colors.fg_] = fg_color_hash;
        MEM_BG_PALETTE[target * 16 + default_colors.bg_] = bg_color_hash;

        set_overlay_tile(x, y, glyph, target);

        custom_text_palette_write_ptr =
            ((target + 1) - custom_text_palette_begin) %
                custom_text_palette_count +
            custom_text_palette_begin;

        if (custom_text_palette_write_ptr == custom_text_palette_begin) {
            warning("wraparound in custom text palette alloc");
        }
    }
}



static void set_map_tile_16p_palette(u8 base, u16 x, u16 y, int palette)
{
    auto ref = [](u16 x_, u16 y_) { return x_ * 2 + y_ * 32 * 2; };

    auto screen_block = [&]() -> u16 {
        // FIXME: technically, our background is 32x16 16p tiles, so we should
        // be adjusting sb index here for x > 15. In practice, SKYLAND uses
        // 16x16 tile maps, so it doesn't matter.
        return base;
    }();

    u16 val = 0;

    val = MEM_SCREENBLOCKS[screen_block][0 + ref(x % 16, y)];
    val &= ~(SE_PALBANK_MASK);
    val |= SE_PALBANK(palette);
    MEM_SCREENBLOCKS[screen_block][0 + ref(x % 16, y)] = val;


    val = MEM_SCREENBLOCKS[screen_block][1 + ref(x % 16, y)];
    val &= ~(SE_PALBANK_MASK);
    val |= SE_PALBANK(palette);
    MEM_SCREENBLOCKS[screen_block][1 + ref(x % 16, y)] = val;


    val = MEM_SCREENBLOCKS[screen_block][0 + ref(x % 16, y) + 32];
    val &= ~(SE_PALBANK_MASK);
    val |= SE_PALBANK(palette);
    MEM_SCREENBLOCKS[screen_block][0 + ref(x % 16, y) + 32] = val;


    val = MEM_SCREENBLOCKS[screen_block][1 + ref(x % 16, y) + 32];
    val &= ~(SE_PALBANK_MASK);
    val |= SE_PALBANK(palette);
    MEM_SCREENBLOCKS[screen_block][1 + ref(x % 16, y) + 32] = val;
}



void Platform::set_flip(Layer layer, u16 x, u16 y, bool xflip, bool yflip)
{
    switch (layer) {
    case Layer::map_1_ext:
        // TODO...
        break;

    case Layer::map_0_ext:
        // TODO...
        break;

    case Layer::overlay: {
        auto c = overlay_back_buffer[x + y * 32];
        if (xflip) {
            c |= 1 << 10;
        } else {
            c &= ~(1 << 10);
        }

        if (yflip) {
            c |= 1 << 11;
        } else {
            c &= ~(1 << 11);
        }

        overlay_back_buffer[x + y * 32] = c;
        overlay_back_buffer_changed = true;
        break;
    }

    default:
        break;
    }
}



void Platform::set_palette(Layer layer, u16 x, u16 y, u16 palette)
{
    if (layer == Layer::map_1_ext) {
        set_map_tile_16p_palette(sbb_t1_tiles, x, y, palette);
    } else if (layer == Layer::map_0_ext) {
        set_map_tile_16p_palette(sbb_t0_tiles, x, y, palette);
    } else if (layer == Layer::overlay) {
        auto t = get_tile(Layer::overlay, x, y);
        set_overlay_tile(x, y, t, palette);
    }
}


u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    if (layer == Layer::overlay) {
        return (overlay_back_buffer[x + y * 32] & (SE_PALBANK_MASK)) >>
               SE_PALBANK_SHIFT;
    }
    fatal("unimplemented get_palette for requested layer");
}



void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
    if (layer == Layer::map_1) {
        MEM_SCREENBLOCKS[sbb_t1_tiles][x + y * 32] = val | SE_PALBANK(2);
    } else if (layer == Layer::map_0) {
        MEM_SCREENBLOCKS[sbb_t0_tiles][x + y * 32] = val | SE_PALBANK(0);
    }
}



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        u16 val,
                        Optional<u16> palette)
{
    switch (layer) {
    case Layer::overlay:
        if (x > 31 or y > 31) {
            return;
        }
        set_overlay_tile(x, y, val, palette ? *palette : 1);
        break;

    case Layer::map_1_ext:
        set_map_tile_16p(sbb_t1_tiles, x, y, val, palette ? *palette : 2);
        break;

    case Layer::map_0_ext:
        set_map_tile_16p(sbb_t0_tiles, x, y, val, palette ? *palette : 0);
        break;

    case Layer::map_1:
        if (x > 15 or y > 19) {
            return;
        }
        set_map_tile(sbb_t1_tiles, x, y, val, 2);
        break;

    case Layer::map_0:
        if (x > 15 or y > 19) {
            return;
        }
        set_map_tile(sbb_t0_tiles, x, y, val, 0);
        break;

    case Layer::background:
        if (x > 31 or y > 32) {
            return;
        }
        MEM_SCREENBLOCKS[sbb_bg_tiles][x + y * 32] = val | SE_PALBANK(11);
        break;
    }
}


////////////////////////////////////////////////////////////////////////////////
// NetworkPeer
////////////////////////////////////////////////////////////////////////////////


Platform::NetworkPeer::NetworkPeer()
{
}


static int multiplayer_is_master()
{
    return (REG_SIOCNT & (1 << 2)) == 0 and (REG_SIOCNT & (1 << 3));
}


// NOTE: you may only call this function immediately after a transmission,
// otherwise, may return a garbage value.
static int multiplayer_error()
{
    return REG_SIOCNT & (1 << 6);
}


// NOTE: This function should only be called in a serial interrupt handler,
// otherwise, may return a garbage value.


static bool multiplayer_validate_modes()
{
    // 1 if all GBAs are in the correct mode, 0 otherwise.
    return REG_SIOCNT & (1 << 3);
}


static bool multiplayer_validate()
{
    if (not multiplayer_validate_modes()) {
        return false;
    } else {
    }
    return true;
}


// The gameboy Multi link protocol always sends data, no matter what, even if we
// do not have any new data to put in the send buffer. Because there is no
// distinction between real data and empty transmits, we will transmit in
// fixed-size chunks. The receiver knows when it's received a whole message,
// after a specific number of iterations. Now, there are other ways, potentially
// better ways, to handle this situation. But this way seems easiest, although
// probably uses a lot of unnecessary bandwidth. Another drawback: the poller
// needs ignore messages that are all zeroes. Accomplished easily enough by
// prefixing the sent message with an enum, where the zeroth enumeration is
// unused.
static const int message_iters =
    Platform::NetworkPeer::max_message_size / sizeof(u16);


struct WireMessage
{
    u16 data_[message_iters] = {};
};


using TxInfo = WireMessage;
using RxInfo = WireMessage;


struct MultiplayerComms
{
    int rx_loss = 0;
    int tx_loss = 0;

    int rx_message_count = 0;
    int tx_message_count = 0;


    static constexpr const int tx_ring_size = 64;
    ObjectPool<TxInfo, tx_ring_size> tx_message_pool;

    int tx_ring_write_pos = 0;
    int tx_ring_read_pos = 0;
    TxInfo* tx_ring[tx_ring_size] = {nullptr};


    static constexpr const int rx_ring_size = 64;
    ObjectPool<RxInfo, rx_ring_size> rx_message_pool;

    int rx_ring_write_pos = 0;
    int rx_ring_read_pos = 0;
    RxInfo* rx_ring[rx_ring_size] = {nullptr};

    int rx_iter_state = 0;
    RxInfo* rx_current_message =
        nullptr; // Note: we will drop the first message, oh well.

    // The multi serial io mode always transmits, even when there's nothing to
    // send. At first, I was allowing zeroed out messages generated by the
    // platform to pass through to the user. But doing so takes up a lot of
    // space in the rx buffer, so despite the inconvenience, for performance
    // reasons, I am going to have to require that messages containing all
    // zeroes never be sent by the user.
    bool rx_current_all_zeroes = true;

    int transmit_busy_count = 0;


    int tx_iter_state = 0;
    TxInfo* tx_current_message = nullptr;

    int null_bytes_written = 0;

    bool is_host = false;

    RxInfo* poller_current_message = nullptr;


    MultiplayerComms()
        : tx_message_pool("transmit-packet-pool"),
          rx_message_pool("receive-packet-pool")
    {
    }
};


static MultiplayerComms multiplayer_comms;


static TxInfo* tx_ring_pop()
{
    auto& mc = multiplayer_comms;


    TxInfo* msg = nullptr;

    for (int i = mc.tx_ring_read_pos; i < mc.tx_ring_read_pos + mc.tx_ring_size;
         ++i) {
        auto index = i % mc.tx_ring_size;
        if (mc.tx_ring[index]) {
            msg = mc.tx_ring[index];
            mc.tx_ring[index] = nullptr;
            mc.tx_ring_read_pos = index;
            return msg;
        }
    }

    mc.tx_ring_read_pos += 1;
    mc.tx_ring_read_pos %= mc.tx_ring_size;

    // The transmit ring is completely empty!
    return nullptr;
}


static void rx_ring_push(RxInfo* message)
{
    auto& mc = multiplayer_comms;

    mc.rx_message_count += 1;

    if (mc.rx_ring[mc.rx_ring_write_pos]) {
        // The reader does not seem to be keeping up!
        mc.rx_loss += 1;

        auto lost_message = mc.rx_ring[mc.rx_ring_write_pos];

        mc.rx_ring[mc.rx_ring_write_pos] = nullptr;
        mc.rx_message_pool.free(lost_message);
    }

    mc.rx_ring[mc.rx_ring_write_pos] = message;
    mc.rx_ring_write_pos += 1;
    mc.rx_ring_write_pos %= mc.rx_ring_size;
}


static RxInfo* rx_ring_pop()
{
    auto& mc = multiplayer_comms;

    RxInfo* msg = nullptr;

    for (int i = mc.rx_ring_read_pos; i < mc.rx_ring_read_pos + mc.rx_ring_size;
         ++i) {
        auto index = i % mc.rx_ring_size;

        if (mc.rx_ring[index]) {
            msg = mc.rx_ring[index];
            mc.rx_ring[index] = nullptr;
            mc.rx_ring_read_pos = index;

            return msg;
        }
    }

    mc.rx_ring_read_pos += 1;
    mc.rx_ring_read_pos %= mc.rx_ring_size;

    return nullptr;
}


static void multiplayer_rx_receive()
{
    auto& mc = multiplayer_comms;

    if (mc.rx_iter_state == message_iters) {
        if (mc.rx_current_message) {
            if (mc.rx_current_all_zeroes) {
                mc.rx_message_pool.free(mc.rx_current_message);
            } else {
                rx_ring_push(mc.rx_current_message);
            }
        }

        mc.rx_current_all_zeroes = true;

        mc.rx_current_message = mc.rx_message_pool.alloc();
        if (not mc.rx_current_message) {
            mc.rx_loss += 1;
        }
        mc.rx_iter_state = 0;
    }

    if (mc.rx_current_message) {
        const auto val =
            multiplayer_is_master() ? REG_SIOMULTI1 : REG_SIOMULTI0;
        if (mc.rx_current_all_zeroes and val) {
            mc.rx_current_all_zeroes = false;
        }
        mc.rx_current_message->data_[mc.rx_iter_state++] = val;

    } else {
        mc.rx_iter_state++;
    }
}


static bool multiplayer_busy()
{
    return REG_SIOCNT & SIO_START;
}



int Platform::NetworkPeer::send_queue_capacity() const
{
    return MultiplayerComms::tx_ring_size;
}



int Platform::NetworkPeer::send_queue_size() const
{
    int result = 0;

    for (auto ptr : multiplayer_comms.tx_ring) {
        if (ptr) {
            ++result;
        }
    }

    return result;
}


bool Platform::NetworkPeer::send_message(const Message& message)
{
    if (message.length_ > sizeof(TxInfo::data_)) {
        ::__platform__->fatal("invalid network packet size");
    }

    if (not is_connected()) {
        return false;
    }

    // TODO: uncomment this block if we actually see issues on the real hardware...
    // if (tx_iter_state == message_iters) {
    //     // Decreases the likelihood of manipulating data shared by the interrupt
    //     // handlers. See related comment in the poll_message() function.
    //     return false;
    // }

    auto& mc = multiplayer_comms;


    if (mc.tx_ring[mc.tx_ring_write_pos]) {
        // The writer does not seem to be keeping up! Guess we'll have to drop a
        // message :(
        mc.tx_loss += 1;

        auto lost_message = mc.tx_ring[mc.tx_ring_write_pos];
        mc.tx_ring[mc.tx_ring_write_pos] = nullptr;

        mc.tx_message_pool.free(lost_message);
    }

    auto msg = mc.tx_message_pool.alloc();
    if (not msg) {
        // error! Could not transmit messages fast enough, i.e. we've exhausted
        // the message pool! How to handle this condition!?
        mc.tx_loss += 1;
        return false;
    }

    __builtin_memcpy(msg->data_, message.data_, message.length_);

    mc.tx_ring[mc.tx_ring_write_pos] = msg;
    mc.tx_ring_write_pos += 1;
    mc.tx_ring_write_pos %= mc.tx_ring_size;

    return true;
}


static void multiplayer_tx_send()
{
    auto& mc = multiplayer_comms;

    if (mc.tx_iter_state == message_iters) {
        if (mc.tx_current_message) {
            mc.tx_message_pool.free(mc.tx_current_message);
            mc.tx_message_count += 1;
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


// We want to wait long enough for the minions to prepare TX data for the
// master.
static void multiplayer_schedule_master_tx()
{
    REG_TM2CNT_H = 0x00C1;
    REG_TM2CNT_L = 65000; // Be careful with this delay! Due to manufacturing
                          // differences between Gameboy Advance units, you
                          // really don't want to get too smart, and try to
                          // calculate the time right up to the boundary of
                          // where you expect the interrupt to happen. Allow
                          // some extra wiggle room, for other devices that may
                          // raise a serial receive interrupt later than you
                          // expect. Maybe, this timer could be sped up a bit,
                          // but I don't really know... here's the thing, this
                          // code CURRENTLY WORKS, so don't use a faster timer
                          // interrupt until you've tested the code on a bunch
                          // different real gba units (try out the code on the
                          // original gba, both sp models, etc.)

    irqEnable(IRQ_TIMER2);
    irqSet(IRQ_TIMER2, [] {
        if (multiplayer_busy()) {
            ++multiplayer_comms.transmit_busy_count;
            return; // still busy, try again. The only thing that should kick
                    // off this timer, though, is the serial irq, and the
                    // initial connection, so not sure how we could get into
                    // this state.
        } else {
            irqDisable(IRQ_TIMER2);
            multiplayer_tx_send();
            REG_SIOCNT = REG_SIOCNT | SIO_START;
        }
    });
}


static void multiplayer_schedule_tx()
{
    // If we're the minion, simply enter data into the send queue. The
    // master will wait before initiating another transmit.
    if (multiplayer_is_master()) {
        multiplayer_schedule_master_tx();
    } else {
        multiplayer_tx_send();
    }
}


static void multiplayer_serial_isr()
{
    if (UNLIKELY(multiplayer_error())) {
        ::__platform__->network_peer().disconnect();
        return;
    }

    multiplayer_comms.is_host = multiplayer_is_master();

    multiplayer_rx_receive();
    multiplayer_schedule_tx();
}


Optional<Platform::NetworkPeer::Message> Platform::NetworkPeer::poll_message()
{
    auto& mc = multiplayer_comms;

    if (mc.rx_iter_state == message_iters) {
        // This further decreases the likelihood of messing up the receive
        // interrupt handler by manipulating shared data. We really should be
        // declaring stuff volatile and disabling interrupts, but we cannot
        // easily do those things, for various practical reasons, so we're just
        // hoping that a problematic interrupt during a transmit or a poll is
        // just exceedingly unlikely in practice. The serial interrupt handler
        // runs approximately twice per frame, and the game only transmits a few
        // messages per second. Furthermore, the interrupt handlers only access
        // shared state when rx_iter_state == message_iters, so only one in six
        // interrupts manipulates shared state, i.e. only one occurrence every
        // three or so frames. And for writes to shared data to even be a
        // problem, the interrupt would have to occur between two instructions
        // when writing to the message ring or to the message pool. And on top
        // of all that, we are leaving packets in the rx buffer while
        // rx_iter_state == message iters, so we really shouldn't be writing at
        // the same time anyway. So in practice, the possibility of manipulating
        // shared data is just vanishingly small, although I acknowledge that
        // it's a potential problem. There _IS_ a bug, but I've masked it pretty
        // well (I hope). No issues detectable in an emulator, but we'll see
        // about the real hardware... once my link cable arrives in the mail.
        // P.S.: Tested on actual hardware, works fine.
        return {};
    }
    if (auto msg = rx_ring_pop()) {
        if (UNLIKELY(mc.poller_current_message not_eq nullptr)) {
            // failure to deallocate/consume message!
            mc.rx_message_pool.free(msg);
            disconnect();
            return {};
        }
        mc.poller_current_message = msg;
        return Platform::NetworkPeer::Message{
            reinterpret_cast<u8*>(msg->data_),
            static_cast<int>(sizeof(WireMessage::data_))};
    }
    return {};
}


void Platform::NetworkPeer::poll_consume(u32 size)
{
    auto& mc = multiplayer_comms;

    if (mc.poller_current_message) {
        mc.rx_message_pool.free(mc.poller_current_message);
    } else {
        ::__platform__->fatal("logic error in net poll");
    }
    mc.poller_current_message = nullptr;
}



static void multiplayer_init()
{
    Microseconds delta = 0;

MASTER_RETRY:
    ::__platform__->network_peer().disconnect();

    ::__platform__->sleep(5);

    REG_RCNT = R_MULTI;
    REG_SIOCNT = SIO_MULTI;
    REG_SIOCNT = REG_SIOCNT | SIO_IRQ | SIO_115200;

    irqEnable(IRQ_SERIAL);
    irqSet(IRQ_SERIAL, multiplayer_serial_isr);

    // Put this here for now, not sure whether it's really necessary...
    REG_SIOMLT_SEND = 0x5555;

    ::__platform__->delta_clock().reset();

    while (not multiplayer_validate()) {
        delta += ::__platform__->delta_clock().reset();
        if (delta > seconds(20)) {
            if (not multiplayer_validate_modes()) {
                error("not all GBAs are in MULTI mode");
            }
            ::__platform__->network_peer()
                .disconnect(); // just for good measure
            REG_SIOCNT = 0;
            irqDisable(IRQ_SERIAL);
            return;
        }
        PLATFORM_EXTENSION(feed_watchdog);
    }

    const char* handshake =
        "lnsk06"; // Link cable program, skyland, 6 byte packet.

    if (strlen(handshake) not_eq Platform::NetworkPeer::max_message_size) {
        ::__platform__->network_peer().disconnect();
        error("handshake string does not equal message size");
        return;
    }

    set_gflag(GlobalFlag::multiplayer_connected, true);

    ::__platform__->network_peer().send_message(
        {(u8*)handshake, sizeof handshake});

    multiplayer_schedule_tx();

    if (multiplayer_is_master()) {
        info("I am the master");
    }

    while (true) {
        PLATFORM_EXTENSION(feed_watchdog);
        delta += ::__platform__->delta_clock().reset();
        if (delta > seconds(20)) {
            StringBuffer<64> err =
                "no valid handshake received within a reasonable window ";
            err += stringify(multiplayer_comms.rx_message_count);
            error(err.c_str());
            ::__platform__->network_peer().disconnect();
            return;
        } else if (auto msg = ::__platform__->network_peer().poll_message()) {
            for (u32 i = 0; i < sizeof handshake; ++i) {
                if (((u8*)msg->data_)[i] not_eq handshake[i]) {
                    if (multiplayer_is_master()) {
                        // For the master, if none of the other GBAs are in
                        // multi serial mode yet, the SIOCNT register will show
                        // that all gbas are in a ready state (all of one
                        // device). The master will, therefore, push out a
                        // message, and receive back garbage data. So we want to
                        // keep retrying, in order to account for the scenario
                        // where the other device is not yet plugged in, or the
                        // other player has not initiated their own connection.
                        info("master retrying...");

                        StringBuffer<32> temp = "got: ";
                        for (u32 i = 0; i < sizeof handshake; ++i) {
                            temp.push_back(((char*)msg->data_)[i]);
                        }
                        info(temp.c_str());

                        // busy-wait for a bit. This is sort of necessary;
                        // Platform::sleep() does not contribute to the
                        // delta clock offset (by design), so if we don't
                        // burn up some time here, we will take a _long_
                        // time to reach the timeout interval.
                        busy_wait(10000);
                        goto MASTER_RETRY; // lol yikes a goto
                    } else {
                        ::__platform__->network_peer().disconnect();
                        info("invalid handshake");
                        return;
                    }
                }
            }
            info("validated handshake");
            ::__platform__->network_peer().poll_consume(sizeof handshake);
            return;
        }
    }
}



void Platform::NetworkPeer::connect(const char* peer)
{
    // If the gameboy player is active, any multiplayer initialization would
    // clobber the Normal_32 serial transfer between the gameboy player and the
    // gameboy advance.
    if (get_gflag(GlobalFlag::gbp_unlocked)) {
        return;
    }

    audio_update_swap(audio_update_fast_cb);

    multiplayer_init();
}


void Platform::NetworkPeer::listen()
{
    if (get_gflag(GlobalFlag::gbp_unlocked)) {
        return;
    }

    multiplayer_init();
}


void Platform::NetworkPeer::update()
{
}


bool Platform::NetworkPeer::is_connected() const
{
    return get_gflag(GlobalFlag::multiplayer_connected);
}


bool Platform::NetworkPeer::is_host() const
{
    return multiplayer_comms.is_host;
}


void Platform::NetworkPeer::disconnect()
{
    // Be very careful editing this function. We need to get ourselves back to a
    // completely clean slate, otherwise, we won't be able to reconnect (e.g. if
    // you leave a message sitting in the transmit ring, it may be erroneously
    // sent out when you try to reconnect, instead of the handshake message);
    if (is_connected()) {

        info("disconnected!");
        set_gflag(GlobalFlag::multiplayer_connected, false);
        irqDisable(IRQ_SERIAL);
        irqDisable(IRQ_TIMER2);
        REG_SIOCNT = 0;

        auto& mc = multiplayer_comms;

        if (mc.poller_current_message) {
            // Not sure whether this is the correct thing to do here...
            mc.rx_message_pool.free(mc.poller_current_message);
            mc.poller_current_message = nullptr;
        }

        mc.rx_iter_state = 0;
        if (mc.rx_current_message) {
            mc.rx_message_pool.free(mc.rx_current_message);
            mc.rx_current_message = nullptr;
        }
        mc.rx_current_all_zeroes = true;
        for (auto& msg : mc.rx_ring) {
            if (msg) {
                StringBuffer<32> note = "consumed msg containing ";
                note.push_back(((char*)msg->data_)[0]);
                note.push_back(((char*)msg->data_)[1]);
                note.push_back(((char*)msg->data_)[2]);
                note.push_back(((char*)msg->data_)[4]);
                note.push_back(((char*)msg->data_)[5]);
                note.push_back(((char*)msg->data_)[6]);
                note += " during disconnect";
                info(note.c_str());
                mc.rx_message_pool.free(msg);
                msg = nullptr;
            }
        }
        mc.rx_ring_write_pos = 0;
        mc.rx_ring_read_pos = 0;

        mc.tx_iter_state = 0;
        if (mc.tx_current_message) {
            mc.tx_message_pool.free(mc.tx_current_message);
            mc.tx_current_message = nullptr;
        }
        for (auto& msg : mc.tx_ring) {
            if (msg) {
                mc.tx_message_pool.free(msg);
                msg = nullptr;
            }
        }
        mc.tx_ring_write_pos = 0;
        mc.tx_ring_read_pos = 0;
    }
}


Platform::NetworkPeer::Interface Platform::NetworkPeer::interface() const
{
    return Interface::serial_cable;
}


Platform::NetworkPeer::~NetworkPeer()
{
    // ...
}



IrqState critical_section_enter()
{
    int temp = 0;

    // Stop dma transfers
    DMA_TRANSFER((volatile short*)0x4000014, &temp, 1, 0, 0);
    DMA_TRANSFER((volatile short*)0x4000016, &temp, 1, 3, 0);

    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 8);
    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 9);

    const u16 ime = REG_IME;
    REG_IME = 0;
    const u16 ie = REG_IE;
    REG_IE = 0;

    return {ime, ie};
}



void critical_section_exit(IrqState state)
{
    REG_IE = state.second;
    REG_IME = state.first;

    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 9);
    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 8);
}



////////////////////////////////////////////////////////////////////////////////
//
// RemoteConsole
//
// Thanks to Ties Stuij, who created a great tutorial about how to correctly
// wire the UART to the GBA. Note that we are not using flow control, so RTS and
// CTS do not need to be connected.
//
// GBA                rs232 usb cable
// ---                ---------------
// 2 SO  red --------> 5 RxD yel
// 3 SI  org <-------- 4 TxD org
// 4 SD  bwn --------> 2 CTS bwn
// 5 SC  grn <-------- 6 RTS grn
// 6 GND blu <-------> 1 GND blk
//
////////////////////////////////////////////////////////////////////////////////


using ConsoleLine = Platform::RemoteConsole::Line;


struct RemoteConsoleState
{
    // NOTE: Some of these vars should probably be volatile, but, as we're
    // dealing with keystrokes, which happen on a human timescale, the
    // probability of anything getting messed up is pretty small.

    Optional<DynamicMemory<ConsoleLine>> rx_in_progress_;
    Buffer<DynamicMemory<ConsoleLine>, 4> rx_full_lines_;

    Optional<DynamicMemory<ConsoleLine>> tx_msg_;
};


static EWRAM_DATA Optional<RemoteConsoleState> remote_console_state;



static bool uart_echo_skip = false;



static void uart_serial_isr()
{
    if (uart_echo_skip) {
        uart_echo_skip = false;
        return;
    }

    auto& state = *::remote_console_state;

    const char data = REG_SIODATA8;

    const bool is_uart_error = REG_SIOCNT & (1 << 6);
    if (is_uart_error) {
        // just for debugging purposes
        ++multiplayer_comms.rx_loss;
        return;
    }

    // NOTE: a later discovery: in addition to is_rcv not working in the
    // commented out code (below), send_ready also does not work. i.e. we cannot
    // receive while we're sending.
    const bool send_ready = !(REG_SIOCNT & 0x0010);
    if (send_ready and state.tx_msg_ and not(*state.tx_msg_)->empty()) {

        // just for debugging purposes
        ++multiplayer_comms.tx_message_count;

        REG_SIODATA8 = *((*state.tx_msg_)->end() - 1);
        (*state.tx_msg_)->pop_back();
        state.rx_in_progress_.reset();
        return;
    } else if (state.tx_msg_ and (*state.tx_msg_)->empty()) {
        state.tx_msg_.reset();
    }

    // FIXME: I'm not seeing the receive_ready flag for some reason, but this
    // code really _should_ be checking the state of the receive flag in the
    // serial control register before reading a byte. What I'm doing here is
    // possibly undefined behavior that happens to work, because my code is
    // ignoring the null bytes.
    //
    // const bool is_rcv = !(REG_SIOCNT & 0x0020);
    // if (is_rcv) {

    if (data == '\r') {
        if (state.rx_in_progress_) {
            state.rx_full_lines_.push_back(std::move(*state.rx_in_progress_));
            state.rx_in_progress_.reset();
        }
    } else if (data == 0x08 /* ASCII backspace */ or
               data == 0x7f /* Strange char used by picocom as a backspace */) {
        if (state.rx_in_progress_) {
            (*state.rx_in_progress_)->pop_back();
        }
    } else if (data == '\0') {
        // Hmm... how'd we end up with a null byte!?
    } else {
        if (not state.rx_in_progress_) {
            state.rx_in_progress_ =
                allocate_dynamic<ConsoleLine>("uart-rx-buffer");
        }

        if (state.rx_in_progress_) {
            (*state.rx_in_progress_)->push_back(data);
        }
    }

    if (data not_eq '\r') {
        if (data == 0x7f) {
            // Why would you send 0x7f but not handle it upon echo? I have no
            // idea why some terminals send a backspace character yet don't
            // understand THE BACKSPACE CHARACTER THAT THEMSELVES SENT! Anyway,
            // I echo back a proper ascii backspace instead, because apparently
            // the developers who wrote these serial consoles were really
            // stupid. Or am I stupid? Someone is clearly an idiot.
            //
            // Description of the problem: In bash, running picocom, I type
            // backspace. GBA receives 0x7f. Whatever, it's not an ascii
            // backspace, but at least it's sort of standardized. I echo back
            // 0x7f, picocom in bash DOES NOTHING.
            REG_SIODATA8 = 0x08;
        } else if (data == 0x04) {
            // Don't echo ctrl-D back to the terminal. Causes problems in some
            // client shells.
            REG_SIODATA8 = '!';
        } else if (data == '\t') {
            // Don't echo back a tab character. The server interprets tabs as
            // autocomplete requests.
        } else {
            REG_SIODATA8 = data;
        }

        // Most serial consoles are super dumb, and assume that the server will
        // echo back everything that they type. If we do the echo now, we'll
        // raise another serial interrupt, which we want to ignore.
        //
        // NOTE: I was relying on local echo for a while, rather than
        // implementing server-side echo. But then, I discovered that Putty
        // echos everything except for newline characters with local echo turned
        // on, and basically, I don't trust the authors of serial terminals to
        // get anything right, based on my experience so far. That's why the
        // server is handling everything and relies on the minimal possible
        // subset of uart functionality. 9600 baud, no local echo, no flow
        // control.
        uart_echo_skip = true;
    }


    // }
}



static void remote_console_start()
{
    ::remote_console_state.emplace();

    irqEnable(IRQ_SERIAL);

    irqSet(IRQ_SERIAL, uart_serial_isr);

    // Stick a character in the data register. This stops the GBA transmitting a
    // Character as soon as it goes into UART mode (?!?)
    // REG_SIODATA8 = 'A';

    // Now to go into UART mode
    REG_RCNT = 0;
    REG_SIOCNT = 0;

    // NOTE: see gba.h for constants
    REG_SIOCNT = SIO_9600 | SIO_UART_LENGTH_8 | SIO_UART_SEND_ENABLE |
                 SIO_UART_RECV_ENABLE | SIO_UART | SIO_IRQ;
}



void Platform::RemoteConsole::start()
{
    remote_console_start();
}



auto Platform::RemoteConsole::peek_buffer() -> Line*
{
    auto& state = *::remote_console_state;
    if (state.rx_in_progress_) {
        return &**state.rx_in_progress_;
    }
    return nullptr;
}



auto Platform::RemoteConsole::readline() -> Optional<Line>
{
    if (not remote_console_state) {
        return {};
    }

    auto& state = *::remote_console_state;

    if (not state.rx_full_lines_.empty()) {
        auto ret = std::move(*state.rx_full_lines_.begin());

        state.rx_full_lines_.erase(state.rx_full_lines_.begin());

        return *ret;
    }
    return {};
}


bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    if (not remote_console_state) {
        return false;
    }

    auto& state = *::remote_console_state;

    if (state.tx_msg_ and not(*state.tx_msg_)->empty()) {
        // TODO: add a queue for output messages! At the moment, there's already
        // a message being written, so we'll need to ignore the input.
        return false;
    }

    state.tx_msg_ = allocate_dynamic<ConsoleLine>("uart-console-output");

    Optional<char> first_char;

    if (*text not_eq '\0') {
        first_char = *text;
        ++text;
    }

    while (*text not_eq '\0') {
        // yeah, this isn't great. We're inserting chars into our buffer in
        // reverse order in the printline call, so that we can simply call
        // pop_back(), which is a constant-time operation, in the more
        // time-critical interrupt handler. Every insert() will essentially
        // memmove what's in the buffer by one byte to the right.
        (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), *text);
        ++text;
    }

    (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), '\r');
    (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), '\n');

    while (*prompt not_eq '\0') {
        (*state.tx_msg_)->insert((*state.tx_msg_)->begin(), *prompt);
        ++prompt;
    }

    if (first_char) {
        // Now that we're done copying the output message, place the first
        // character in the uart shift register, to kick off the send.
        REG_SIODATA8 = *first_char;
    } else {
        auto first = *((*state.tx_msg_)->end() - 1);
        (*state.tx_msg_)->pop_back();
        REG_SIODATA8 = first;
    }

    return true;
}



void download_dlc_blob(Vector<char>&);



static void uart_blocking_output_mode()
{
    REG_SIOCNT = SIO_9600 | SIO_UART | SIO_UART_LENGTH_8 | SIO_UART_SEND_ENABLE;

    irqDisable(IRQ_SERIAL);
}



static inline void uart_blocking_send_sync()
{
    while (REG_SIOCNT & 0x0010)
        ;
}



static void uart_blocking_write(char c)
{
    uart_blocking_send_sync();
    REG_SIODATA8 = c;
}



void CpuFastSet(const void* source, void* dest, u32 mode)
{
    SystemCall(12);
}



void setup_hardcoded_palettes()
{
    // NOTE: these colors were a custom hack I threw in during the GBA game jam,
    // when I wanted background tiles to flicker between a few different colors.
    for (int i = 0; i < 16; ++i) {
        MEM_BG_PALETTE[(15 * 16) + i] =
            agb_color_correction(Color(custom_color(0xef0d54))).bgr_hex_555();
    }
    for (int i = 0; i < 16; ++i) {
        MEM_BG_PALETTE[(14 * 16) + i] =
            agb_color_correction(Color(custom_color(0x103163))).bgr_hex_555();
    }
    for (int i = 0; i < 16; ++i) {
        MEM_BG_PALETTE[(13 * 16) + i] =
            agb_color_correction(Color(ColorConstant::silver_white))
                .bgr_hex_555();
    }

    // Really bad hack. We added a feature where the player can design his/her
    // own flag, but we frequently switch color palettes when viewing
    // interior/exterior of a castle, so we reserve one of the palette banks for
    // the flag palette, which is taken from the tilesheet texture.
    for (auto& info : tile_textures) {
        if (str_eq(info.name_, "tilesheet")) {
            for (int i = 0; i < 16; ++i) {
                auto c = agb_color_correction(
                             Color::from_bgr_hex_555(info.palette_data_[i]))
                             .bgr_hex_555();
                MEM_BG_PALETTE[(12 * 16) + i] = c;
                custom_flag_palette[i] = c;

                // When we started allowing players to design custom sprites, we
                // needed to reserve a sprite palette and fill it with the same
                // color values as the image editor uses for custom tile
                // graphics.
                MEM_PALETTE[16 + i] = info.palette_data_[i];
            }
        }
    }
    for (auto& info : overlay_textures) {
        if (str_eq(info.name_, "overlay_dialog")) {
            // FIXME!!! handle this dynamically, rather than reserving a
            // specific palette...
            for (int i = 0; i < 16; ++i) {
                auto c = agb_color_correction(
                             Color::from_bgr_hex_555(info.palette_data_[i]))
                             .bgr_hex_555();
                MEM_BG_PALETTE[(10 * 16) + i] = c;
            }
        }
    }
}



static const Platform::Extensions extensions{
    .stack_check = []() -> bool {
        if (not canary_check()) {
            longjmp(stack_overflow_resume_context, 1);
        }
        return true;
    },
    .palette_sync =
        []() {
            memcpy32(bg_palette_back_buffer + 32, tilesheet_1_palette, 8);
            memcpy32(bg_palette_back_buffer, tilesheet_0_palette, 8);
            memcpy32(&bg_palette_back_buffer[16 * 11], background_palette, 8);
            set_gflag(GlobalFlag::partial_palette_sync, true);
        },
    .feed_watchdog = []() { ::watchdog_counter = 0; },
    .update_parallax_r1 =
        [](u8 scroll) {
            auto& screen = PLATFORM.screen();
            auto center = screen.get_view().int_center().cast<s32>();
            if (not get_gflag(GlobalFlag::v_parallax)) {
                auto offset = center.y / 2;
                for (int i = 112 - offset; i < 128 - offset; ++i) {
                    u8 temp = scroll + center.x / 3;
                    parallax_table[i] = temp;
                }

                for (int i = 0; i < 112 - offset; ++i) {
                    parallax_table[i] = 0;
                }

                // Fixme: clean up this code...
                return;
            }

            auto offset = center.y / 4 + 3;

            const int x_amount = scroll + (center.x / 3) * 0.8f;

            for (int i = (112 - offset) - 30; i < 128 - offset; ++i) {
                parallax_table[i] = x_amount;
                vertical_parallax_table[i] = offset;
            }

            if (not get_gflag(GlobalFlag::palette_sync)) {
                // NOTE: The palette sync is costly, don't bother to scroll this
                // stuff if we're about to copy over the palette back buffer. During
                // fades, the palettes are copied infrequently anyway.

                s16 far_x_offset = center.x / 4 + 3;

                s16 v_scroll = (offset * 6) / 2 + 24;

                // Gradient effect:
                for (int i = 0; i < (112 - offset) - 30; ++i) {
                    parallax_table[i] = far_x_offset / 4;
                    if (i < -v_scroll) {
                        // For scroll wrapping: We're doing a gradient effect here,
                        // if the dithered tile gradient scrolls such that the
                        // scroll wraps, then fill in the wrapped rows with the
                        // pixels with zero offset scanlines, which use the darkest
                        // gradient color. NOTE: this works because the amount of
                        // background scrolling for the gradient does not exceed the
                        // width of the darkest band of tiles in the gradient.

                        vertical_parallax_table[i] = 0;

                        if (i > 12) {
                            // Yeah, some amount of trial and error here.
                            vertical_parallax_table[i] = v_scroll / 2;
                        }

                    } else {
                        vertical_parallax_table[i] = v_scroll;
                    }
                }
            }
        },
    .update_parallax_r2 =
        [](u8 scroll) {
            auto& screen = PLATFORM.screen();
            auto center = screen.get_view().int_center().cast<s32>();
            if (not get_gflag(GlobalFlag::v_parallax)) {
                auto offset = center.y / 2;
                for (int i = 128 - offset; i < 160 - offset; ++i) {
                    u8 temp = scroll + center.x / 3;
                    parallax_table[i] = temp;
                }
                return;
            }

            const int offset = center.y / 2 * 0.7f + 3;

            const auto x_amount = scroll + center.x / 3;

            for (int i = 128 - offset; i < 160 - offset; ++i) {
                parallax_table[i] = x_amount;
                vertical_parallax_table[i] = offset;
            }

            // When the two layers of parallax scrolling diverge, there is a gap of
            // unshifted pixels between them, which we need to account for.
            // Otherwise, certain rows that were scrolled last time will not have
            // their y-scroll adjusted, which can create graphical glitches.
            auto other_row_offset = center.y / 4 + 3;

            for (int i = 128 - other_row_offset; i < (128 - offset); ++i) {
                // We put a layer of solid-colored tiles offscreen, and we scroll
                // them up to fill the gap.
                vertical_parallax_table[i] = 38;
                parallax_table[i] = x_amount;
            }
        },
    .update_parallax_macro =
        [](int scroll) {
            for (int i = 0; i < 160; ++i) {
                vertical_parallax_table[i] = 0;
            }
            int amount_4 = (scroll * 0.1f);
            for (int i = 32; i < 64; ++i) {
                parallax_table[i] = (u8)amount_4;
            }
            int amount_3 = (scroll * 0.4f);
            for (int i = 64; i < 100; ++i) {
                parallax_table[i] = (u8)amount_3;
            }
            int amount_2 = (scroll * 0.7f);
            for (int i = 100; i < 128; ++i) {
                parallax_table[i] = (u8)amount_2;
            }
            for (int i = 128; i < 160; ++i) {
                parallax_table[i] = (u8)scroll;
            }
        },
    .enable_parallax_clouds =
        [](bool on) {
            set_gflag(GlobalFlag::parallax_clouds, on);

            if (on) {
                vblank_dma_callback = vblank_full_transfer_scroll_isr;
                for (int i = 0; i < 280; ++i) {
                    if (i < 140) {
                        vertical_parallax_table[i] = 200;
                    } else {
                        vertical_parallax_table[i] = 0;
                    }
                }
            }
        },
    .vertical_parallax_enable =
        [](bool on) {
            set_gflag(GlobalFlag::v_parallax, on);

            if (on) {
                vblank_dma_callback = vblank_full_transfer_scroll_isr;
            } else {
                vblank_dma_callback = vblank_horizontal_transfer_scroll_isr;
            }
        },
    .force_vsync = [] { VBlankIntrWait(); },
    .overlay_circle_effect =
        [](int radius, int x, int y) {
            if (radius == 0 and opt_dma_buffer_) {
                // Cancel DMA transfer. Important, because we're freeing the buffer
                // of data used by the hdma when we drop the opt_dma_buffer.
                DMA_TRANSFER(&REG_WIN0H, &vertical_parallax_table[1], 1, 2, 0);
                vblank_dma_callback = vblank_full_transfer_scroll_isr;
                opt_dma_buffer_.reset();
                PLATFORM.fill_overlay(0);
                window_init_default();
            } else if (radius not_eq 0) {
                if (not opt_dma_buffer_) {
                    VBlankIntrWait();
                    opt_dma_buffer_ =
                        allocate_dynamic<OptDmaBufferData>("opt-dma-buffer");
                    memset((*opt_dma_buffer_)->data(), 0, 160 * 2);
                    window_init_effectmode();
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 8);
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 9);
                    win_circle((*opt_dma_buffer_)->data(), x, y, radius);
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 9);
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 8);
                    DMA_TRANSFER(
                        &REG_WIN0H, (*opt_dma_buffer_)->data(), 1, 2, DMA_HDMA);
                    PLATFORM.fill_overlay(491);
                }
                vblank_dma_callback = vblank_circle_effect_isr;
                dma_effect_params[0] = radius;
                dma_effect_params[1] = x;
                dma_effect_params[2] = y;
            }
        },
    .iris_wipe_effect =
        [](int radius, int x, int y) {
            if (radius == 0 and opt_dma_buffer_) {
                // Cancel DMA transfer. Important, because we're freeing the buffer
                // of data used by the hdma when we drop the opt_dma_buffer.
                DMA_TRANSFER(&REG_WIN0H, &vertical_parallax_table[1], 1, 2, 0);
                vblank_dma_callback = vblank_full_transfer_scroll_isr;
                opt_dma_buffer_.reset();
                PLATFORM.fill_overlay(0);
                window_init_default();
            } else if (radius not_eq 0) {
                if (not opt_dma_buffer_) {
                    VBlankIntrWait();
                    opt_dma_buffer_ =
                        allocate_dynamic<OptDmaBufferData>("opt-dma-buffer");
                    memset((*opt_dma_buffer_)->data(), 0, 160 * 2);
                    window_init_inverse_effectmode();
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 8);
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 9);
                    win_circle((*opt_dma_buffer_)->data(), x, y, radius);
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 9);
                    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 8);
                    DMA_TRANSFER(
                        &REG_WIN0H, (*opt_dma_buffer_)->data(), 1, 2, DMA_HDMA);
                    PLATFORM.fill_overlay(112);
                }
                vblank_dma_callback = vblank_circle_effect_isr;
                dma_effect_params[0] = radius;
                dma_effect_params[1] = x;
                dma_effect_params[2] = y;
            }
        },
    .hibernate =
        [] {
            REG_KEYCNT =
                KEY_SELECT | KEY_R | KEY_L | KEYIRQ_ENABLE | KEYIRQ_AND;

            irqSet(IRQ_KEYPAD, [] {});

            Stop();

            irqSet(IRQ_KEYPAD, keypad_isr);
            REG_KEYCNT = KEY_SELECT | KEY_START | KEY_R | KEY_L |
                         KEYIRQ_ENABLE | KEYIRQ_AND;
        },
    .print_memory_diagnostics = [] { scratch_buffer_memory_diagnostics(); },
    .console_write_buffer =
        [](Vector<char>& input) {
            uart_blocking_output_mode();

            // Our console doesn't support output for lines larger than ~2
            // kilobytes. This system call implements a blocking write for large
            // data buffers.

            for (char c : input) {
                uart_blocking_write(c);
            }

            uart_blocking_send_sync();
            REG_SIOCNT = 0;
            REG_SIODATA8 = '\n';

            // Re-enable the async non-blocking console.
            remote_console_start();
        },
    .dlc_download = [](Vector<char>& output) { download_dlc_blob(output); },
    .watchdog_on = [] { set_gflag(GlobalFlag::watchdog_disabled, false); },
    .watchdog_off = [] { set_gflag(GlobalFlag::watchdog_disabled, true); },
    .get_stack_usage = [] { return max_stack_usage(); },
    .get_stack_size = [] { return stack_reserved_size(); },
    .restart = [] { ::restart(); },
    .apply_color_correction =
        [](const char* table_name) {
            if (table_name) {
                if (auto fd = PLATFORM.load_file(
                        "",
                        format("/scripts/data/color/%", table_name).c_str());
                    fd.second >= 98304) {
                    color_correction_lut = (u8*)fd.first;
                }
            } else {
                color_correction_lut = nullptr;
            }
            setup_hardcoded_palettes();
        },
    .enable_translucence =
        [](const Buffer<Layer, 4>& layers) {
            u16 bld = 0;

            for (auto& l : layers) {
                switch (l) {
                case Layer::map_0_ext:
                    bld |= BLD_BG0;
                    break;

                case Layer::map_1_ext:
                    bld |= BLD_BG3;
                    break;

                default:
                    break;
                }
            }

            int mode = 0;
            if (layers.size()) {
                mode = 1;
            }

            REG_BLENDCNT = BLD_BUILD(bld, BLD_BG0 | BLD_BG1 | BLD_BG3, mode);
        },
    .psg_play_note =
        [](Platform::Speaker::Channel channel,
           Platform::Speaker::NoteDesc note_desc) {
            auto note = note_desc.regular_.note_;
            u8 octave = note_desc.regular_.octave_;

            if (channel == Platform::Speaker::Channel::noise and
                note_desc.noise_freq_.frequency_select_ == 0) {
                return;
            } else if (channel not_eq Platform::Speaker::Channel::noise and
                       ((u8)note >= (u8)Platform::Speaker::Note::count or
                        note == Platform::Speaker::Note::invalid)) {
                return;
            }


            // Turn off directsound!
            REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 8);
            REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 9);

            switch (channel) {
            case Platform::Speaker::Channel::square_1:
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LSQR1;
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RSQR1;
                analog_channel[(int)channel].last_note_ = note;
                analog_channel[(int)channel].last_octave_ = octave;
                analog_channel[(int)channel].effect_timer_ = 0;
                REG_SND1FREQ = SFREQ_RESET | SND_RATE((u8)note, octave);
                break;

            case Platform::Speaker::Channel::square_2:
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LSQR2;
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RSQR2;
                analog_channel[(int)channel].last_note_ = note;
                analog_channel[(int)channel].last_octave_ = octave;
                analog_channel[(int)channel].effect_timer_ = 0;
                REG_SND2FREQ = SFREQ_RESET | SND_RATE((u8)note, octave);
                break;

            case Platform::Speaker::Channel::noise: {
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LNOISE;
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RNOISE;
                analog_channel[(int)channel].last_note_ = note;
                analog_channel[(int)channel].last_octave_ = octave;
                analog_channel[(int)channel].effect_timer_ = 0;
                auto freq = note_desc.noise_freq_.frequency_select_;
                auto entry = noise_frequency_table_[freq];
                REG_SND4FREQ = 0;
                REG_SND4FREQ = SFREQ_RESET | ((0x0f & entry.shift_) << 4) |
                               (0x07 & entry.ratio_) |
                               (note_desc.noise_freq_.wide_mode_ << 3);
                break;
            }

            case Platform::Speaker::Channel::wave:
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LWAVE;
                REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RWAVE;
                analog_channel[(int)channel].last_note_ = note;
                analog_channel[(int)channel].last_octave_ = octave;
                analog_channel[(int)channel].effect_timer_ = 0;
                break;

            default:
                // TODO...
                break;
            }
        },
    .psg_stop_note =
        [](Platform::Speaker::Channel channel) {
            switch (channel) {
            case Platform::Speaker::Channel::square_1:
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 8);
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xc);
                break;

            case Platform::Speaker::Channel::square_2:
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 9);
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xd);
                break;

            case Platform::Speaker::Channel::noise:
                // FIXME!?
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~SDMG_LNOISE;
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~SDMG_RNOISE;
                break;

            case Platform::Speaker::Channel::wave:
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xb);
                REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xf);
                break;

            default:
                // TODO!
                break;
            }

            // Turn directsound back on!
            REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 9);
            REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 8);
        },
    .psg_apply_effect =
        [](Platform::Speaker::Channel channel,
           Platform::Speaker::Effect effect,
           u8 argument,
           Microseconds delta) {
            if (channel == Platform::Speaker::Channel::invalid) {
                return;
            }


            const auto ch_num = (int)channel;


            auto apply_vibrato = [&](volatile u16* freq_register) {
                auto amplitude = argument & 0x0f;
                auto freq = (argument & 0xf0) >> 4;

                // We're using freq as a divisor. Zero isn't valid
                freq++;

                analog_channel[ch_num].effect_timer_ += delta;
                auto rate = SND_RATE(analog_channel[ch_num].last_note_,
                                     analog_channel[ch_num].last_octave_);


                auto vib =
                    float(cosine(analog_channel[ch_num].effect_timer_ / freq)) /
                    std::numeric_limits<s16>::max();

                vib *= (amplitude << 2);
                rate += vib;
                *freq_register = *freq_register & ~SFREQ_RATE_MASK;
                *freq_register = *freq_register | rate;
            };


            auto apply_duty = [&](volatile u16* ctrl_register) {
                *ctrl_register = *ctrl_register & ~SSQR_DUTY_MASK;
                // (Only four possible duty cycles, hence the mask)
                *ctrl_register =
                    *ctrl_register | SSQR_DUTY((argument >> 4) & 0x03);
            };


            auto apply_envelope = [&](volatile u16* ctrl_register) {
                auto duty =
                    (*ctrl_register & SSQR_DUTY_MASK) >> SSQR_DUTY_SHIFT;
                auto length =
                    (*ctrl_register & SSQR_LEN_MASK) >> SSQR_LEN_SHIFT;

                // To match LSDJ: first nibble: volume, second nibble: 1-7: release with
                // decreasing envelope, 8-f: release with increasing envelope.

                int dir = 0;

                if ((argument & 0x0f) < 8) {
                    dir = 0; // decreasing
                } else {
                    dir = 1;
                }

                *ctrl_register = SSQR_BUILD((argument & 0xf0) >> 4,
                                            dir,
                                            (argument & 0x07),
                                            duty,
                                            length);
            };


            auto cancel_effect = [&](volatile u16* freq_register) {
                *freq_register = *freq_register & ~SFREQ_RATE_MASK;
                *freq_register = *freq_register |
                                 SND_RATE(analog_channel[ch_num].last_note_,
                                          analog_channel[ch_num].last_octave_);

                analog_channel[ch_num].effect_timer_ = 0;
            };


            switch (channel) {
            case Platform::Speaker::Channel::square_1: {
                switch (effect) {
                case Platform::Speaker::Effect::vibrato:
                    apply_vibrato(&REG_SND1FREQ);
                    break;

                case Platform::Speaker::Effect::none:
                    cancel_effect(&REG_SND1FREQ);
                    break;

                case Platform::Speaker::Effect::duty:
                    apply_duty(&REG_SND1CNT);
                    break;

                case Platform::Speaker::Effect::envelope:
                    apply_envelope(&REG_SND1CNT);
                    break;
                }
                break;
            }

            case Platform::Speaker::Channel::square_2:
                switch (effect) {
                case Platform::Speaker::Effect::vibrato:
                    apply_vibrato(&REG_SND2FREQ);
                    break;

                case Platform::Speaker::Effect::none:
                    cancel_effect(&REG_SND2FREQ);
                    break;

                case Platform::Speaker::Effect::duty:
                    apply_duty(&REG_SND2CNT);
                    break;

                case Platform::Speaker::Effect::envelope:
                    apply_envelope(&REG_SND2CNT);
                    break;
                }
                break;

            case Platform::Speaker::Channel::noise:
                switch (effect) {
                case Platform::Speaker::Effect::duty:
                    apply_duty(&REG_SND4CNT);
                    break;

                case Platform::Speaker::Effect::envelope:
                    apply_envelope(&REG_SND4CNT);
                    break;

                default:
                    break;
                }
                break;

            default:
                // TODO...
                break;
            }
        },
    .psg_init_square_1 =
        [](Platform::Speaker::ChannelSettings settings) {
            REG_SND1CNT = SSQR_BUILD(settings.volume_,
                                     settings.envelope_direction_,
                                     settings.envelope_step_,
                                     settings.duty_,
                                     settings.length_);
        },
    .psg_init_square_2 =
        [](Platform::Speaker::ChannelSettings settings) {
            REG_SND2CNT = SSQR_BUILD(settings.volume_,
                                     settings.envelope_direction_,
                                     settings.envelope_step_,
                                     settings.duty_,
                                     settings.length_);
        },
    .psg_init_wave =
        [](Platform::Speaker::ChannelSettings s) {

        },
    .psg_init_noise =
        [](Platform::Speaker::ChannelSettings settings) {
            REG_SND4CNT = SSQR_BUILD(settings.volume_,
                                     settings.envelope_direction_,
                                     settings.envelope_step_,
                                     settings.duty_,
                                     settings.length_);
        },
    .rotate_palette =
        [](Layer l, u8 range_start, u8 range_end) {
            if (l == Layer::map_0_ext) {
                auto prev = MEM_BG_PALETTE[range_end - 1];
                for (int i = range_start; i < range_end; ++i) {
                    auto temp = MEM_BG_PALETTE[i];
                    MEM_BG_PALETTE[i] = prev;
                    prev = temp;
                }
            }
        },
    .__test_compare_sound =
        [](const char* name) {
            if (auto s = get_sound(name)) {

                auto f_info = filesystem::load(
                    format("/scripts/data/sounds/%", name).c_str(), sounds_dir);

                if (std::get<1>(f_info)) {
                    for (int i = 0;
                         i < s->length_ and i < (int)std::get<1>(f_info);
                         ++i) {
                        if (std::get<0>(f_info)[i] not_eq s->data_[i]) {
                            return false;
                        }
                    }
                }

                return true;
            }
            return true;
        },
};



const Platform::Extensions& Platform::get_extensions()
{
    return extensions;
}



void Platform::Speaker::start()
{
    audio_start();
    clear_music();
    play_music("unaccompanied_wind", 0);
}



Platform::Platform()
{
    ::__platform__ = this;

    logger().set_threshold(Severity::fatal);

    if (skyland_mgba_invoke_command(Command::identify, "HELLO.") ==
        "ACKNOWLEDGED.") {
        info("skyland-mgba detected!");
    }


    keyboard().poll();

    Conf conf;

    static const char* conf_section = "hardware.gameboy_advance";

#define CONF_BOOL(NAME)                                                        \
    const bool NAME = *conf.expect<Conf::String>(conf_section, #NAME) == "ye"  \
                                                                         "s";

    CONF_BOOL(detect_repro_flash);

    if (detect_repro_flash) {
        // Check to see if we're running with a bootleg cart.
        bootleg_flash_type = bootleg_get_flash_type();
        switch (bootleg_flash_type) {
        case 1:
        case 2:
        case 3:
            info(format("Repro cart detected! (type %)", bootleg_flash_type));

            // These bootleg flashcarts place save data in flash memory
            // alongside the ROM. Some of these things retail at $2, I'm
            // guessing the manufacturers are trying to cut costs on the sram
            // battery. The carts still use SRAM, and data persisted to flash
            // needs to be copied back to sram at startup.
            //
            // But! InsideGadgets brand flashcarts use similar flash chips and
            // have actual battery-backed SRAM or persistent FRAM. TODO: I guess
            // I need to look into the contents of sram upon startup to see if
            // valid save data already exists.
            //
            // But... resetting rom with L+R+START+SELECT would also cause code
            // to reach this point.

            bootleg_cart_init_sram();
            break;

        default:
            bootleg_flash_type = 0;
            // NOT DETECTED
            break;
        }
    } else {
        bootleg_flash_type = 0;
    }

    if (mgba_detect()) {
        info("running in mgba (or similar)...");
    }

    // Not sure how else to determine whether the cartridge has sram, flash, or
    // something else. An sram write will fail if the cartridge ram is flash, so
    // attempt to save, and if the save fails, assume flash. I don't really know
    // anything about the EEPROM hardware interface...

    CONF_BOOL(detect_flash);
    if (detect_flash) {
        static const u32 sram_test_const = 0xAAAAAAAA;
        sram_save(&sram_test_const, 0, sizeof sram_test_const);

        u32 sram_test_result = 0;
        sram_load(&sram_test_result, 0, sizeof sram_test_result);


        if (sram_test_result not_eq sram_test_const) {
            set_gflag(GlobalFlag::save_using_flash, true);
            info("SRAM write failed, falling back to FLASH");

            ::save_capacity = flash_capacity();
        } else {
            ::save_capacity =
                conf.expect<Conf::Integer>(conf_section, "sram_capacity");
        }
    } else {
        ::save_capacity =
            conf.expect<Conf::Integer>(conf_section, "sram_capacity");
    }


    {
        StringBuffer<32> used("iwram used: ");
        used += stringify(&__data_end__ - &__iwram_start__);
        info(used.c_str());

        used = "ewram used: ";
        used += stringify(&__eheap_start - &__ewram_start);
        info(used.c_str());

        used = "estimated stack size: ";
        used += stringify(stack_reserved_size());
        info(used.c_str());
    }

    // IMPORTANT: No calls to map_glyph() are allowed before reaching this
    // line. Otherwise, the glyph table has not yet been constructed.

    info("Verifying BIOS...");

    const auto bios_version = BiosCheckSum();
    switch (bios_version) {
    case BiosVersion::NDS:
        info("BIOS matches Nintendo DS");
        break;
    case BiosVersion::GBA:
        info("BIOS matches GAMEBOY Advance");
        break;
    default:
        warning("BIOS checksum failed, may be corrupt");
        break;
    }


    irqInit(); // NOTE: Do not move these lines with respect to
               // unlock_gameboy_player(), or you could break the rumble
               // unlocking.

    irqEnable(IRQ_VBLANK);

    irqSet(IRQ_TIMER3, [] {
        delta_total += 0xffff;

        REG_TM3CNT_H = 0;
        REG_TM3CNT_L = 0;
        REG_TM3CNT_H = 1 << 7 | 1 << 6;
    });
    delta_clock().reset();

    irqSet(IRQ_VBLANK, vblank_isr);

    CONF_BOOL(show_epilepsy_warning);
    if (show_epilepsy_warning) {
        show_health_and_safety_message();
    }

    CONF_BOOL(detect_gbp);
    if (detect_gbp and unlock_gameboy_player()) {
        info("gameboy player unlocked!");

        set_gflag(GlobalFlag::gbp_unlocked, true);
        set_gflag(GlobalFlag::rtc_faulty, true);

        RumbleGBPConfig conf{[](void (*rumble_isr)(void)) {
            irqEnable(IRQ_SERIAL);
            irqSet(IRQ_SERIAL, rumble_isr);
        }};

        rumble_init(&conf);

    } else {
        info("gbp not detected");

        rumble_init(nullptr);
    }

    setup_hardcoded_palettes();

    irqSet(IRQ_KEYPAD, keypad_isr);
    irqEnable(IRQ_KEYPAD);
    REG_KEYCNT =
        KEY_SELECT | KEY_START | KEY_R | KEY_L | KEYIRQ_ENABLE | KEYIRQ_AND;

    VBlankIntrWait();
    screen().fade(1, ColorConstant::silver_white);
    init_video(screen());

    fill_overlay(0);

    bool is_gameboy_micro = false;

    CONF_BOOL(ewram_overclock);
    if (ewram_overclock and bios_version not_eq BiosVersion::NDS) {
        if (not ram_overclock()) {
            is_gameboy_micro = true;
        }
    }

    // Fix for the garbage MyBoy! emulator in Android, which, at time of
    // writing, wipes out the value of REG_DISPCNT when overclocking ewram.
    init_video(screen());

    for (u32 i = 0; i < Screen::sprite_limit; ++i) {
        // This was a really insidious bug to track down! When failing to hide
        // unused attributes in the back buffer, the uninitialized objects punch
        // a 1 tile (8x8 pixel) hole in the top left corner of the overlay
        // layer, but not exactly. The tile in the high priority background
        // layer still shows up, but lower priority sprites show through the top
        // left tile, I guess I'm observing some weird interaction involving an
        // overlap between a priority 0 tile and a priority 1 sprite: when a
        // priority 1 sprite is sandwitched in between the two tile layers, the
        // priority 0 background tiles seems to be drawn behind the priority 1
        // sprite. I have no idea why!
        object_attribute_back_buffer[i].attribute_2 = ATTR2_PRIORITY(3);
        object_attribute_back_buffer[i].attribute_0 |= attr0_mask::disabled;
    }


    if (not filesystem::is_mounted()) {
        keyboard_.poll();
        fatal("resource bundle missing");
    }

    CONF_BOOL(fast_waitstates)

    // NOTE: Non-sequential 8 and sequential 3 seem to work well for Cart 0 wait
    // states, although setting these options unmasks a few obscure audio bugs,
    // the game displays visibly less tearing. The cartridge prefetch unmasks
    // even more aggressive audio bugs, and doesn't seem to grant obvious
    // performance benefits, so I'm leaving the cartridge prefetch turned off...
    if (fast_waitstates) {
        // Although there is less tearing when running with optimized
        // waitstates, I actually prefer the feature turned off. I really tuned
        // the feel of the controls before I knew about waitstates, and
        // something just feels off to me when turning this feature on. The game
        // is almost too smooth.
        REG_WAITCNT = 0b0000000000011011;
        info("enabled optimized waitstates...");
    }

    CONF_BOOL(prefetch);

    if (prefetch) {
        REG_WAITCNT |= 1 << 14;
    }

    set_gflag(GlobalFlag::rtc_faulty, true);

    const auto stk_size_min = 11000;
    if (stack_reserved_size() < stk_size_min) {
        Platform::fatal(format("stack size % smaller than suggested %. "
                               "Not a strict requirement, just that I tested "
                               "the game with a stack of this size.",
                               stack_reserved_size(),
                               stk_size_min)
                            .c_str());
    }

    if (&__eheap_start - &__ewram_start > 262143) {
        Platform::fatal("EWRAM Usage exceeds hardware maximum!");
    }


    if (keyboard().pressed<Key::alt_1>() and
        keyboard().pressed<Key::alt_2>() and
        keyboard().pressed<Key::action_1>() and
        keyboard().pressed<Key::select>()) {

        erase_save_sector();
    }

    if (get_gflag(GlobalFlag::gbp_unlocked)) {
        model_id = ModelId::gbplayer;
    } else if (bios_version == BiosVersion::NDS) {
        model_id = ModelId::nds;
    } else if (is_gameboy_micro) {
        model_id = ModelId::gbmicro;
    } else {
        model_id = ModelId::gba;
    }
}



u16 mb_exchange(u16 value)
{
    while (REG_SIOCNT & SIO_START)
        ;
    REG_SIOMLT_SEND = value;
    REG_SIOCNT = REG_SIOCNT | SIO_START;
    while (REG_SIOCNT & SIO_START)
        ;
    u16 result = REG_SIOMULTI1;
    return result;
}


#endif // __GBA__
