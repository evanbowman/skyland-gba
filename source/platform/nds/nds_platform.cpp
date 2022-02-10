// NOTICE: I borrowed parts of libnds from devkit pro, and for some parts I
// wrote my own implementations. Here's the libnds license:
//
//
// Copyright (C) 2005 - 2008
//   Michael Noland (joat)
//   Jason Rogers (dovoto)
//   Dave Murpy (WinterMute)
//
//  This software is provided 'as-is', without any express or implied
//  warranty.  In no event will the authors be held liable for any
//  damages arising from the use of this software.
//
//  Permission is granted to anyone to use this software for any
//  purpose, including commercial applications, and to alter it and
//  redistribute it freely, subject to the following restrictions:
//
//  1. The origin of this software must not be misrepresented; you
//     must not claim that you wrote the original software. If you use
//     this software in a product, an acknowledgment in the product
//     documentation would be appreciated but is not required.
//  2. Altered source versions must be plainly marked as such, and
//     must not be misrepresented as being the original software.
//  3. This notice may not be removed or altered from any source
//     distribution.
//


#include "platform/platform.hpp"
#include "/opt/devkitpro/libnds/include/nds.h"
#include "images.cpp"
#include "platform/color.hpp"



// Implementation of platform for nds



void start(Platform&);



static Platform* platform;



int main(int argc, char** argv)
{
    Platform pfrm;

    start(pfrm);
}



Platform::DeviceName Platform::device_name() const
{
    return "NintendoDS";
}



Platform::TilePixels Platform::extract_tile(Layer layer, u16 tile)
{
    return {};
}



Platform::EncodedTile Platform::encode_tile(u8 tile_data[16][16])
{
    return {};
}



void Platform::overwrite_t0_tile(u16 index, const EncodedTile& t)
{
    // TODO...
}



void Platform::overwrite_t1_tile(u16 index, const EncodedTile& t)
{
    // TODO...
}



void Platform::overwrite_sprite_tile(u16 index, const EncodedTile& t)
{
    // TODO...
}



std::optional<DateTime> Platform::startup_time() const
{
    return {};
}



const char* Platform::get_opt(char opt)
{
    return nullptr;
}



void Platform::fatal(const char* message)
{
    consoleDemoInit();

    iprintf(message);

    while (true) {
        platform->screen().clear();
    }
}



void Platform::restart()
{
    while (true) ;
}



void* Platform::system_call(const char* feature_name, void* arg)
{
    // TODO...

    return nullptr;
}



TileDesc Platform::map_glyph(const utf8::Codepoint& glyph,
                             const TextureMapping& mapping)
{
    return 111;
}



void Platform::load_overlay_chunk(TileDesc dst, TileDesc src, u16 count)
{
    // TODO...
}



std::optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    return {};
}



void Platform::enable_glyph_mode(bool enabled)
{
    // TODO...
}



void Platform::enable_expanded_glyph_mode(bool enabled)
{
    // TODO...
}



void Platform::set_palette(Layer layer, u16 x, u16 y, u16 palette)
{
    // TODO...
}



u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    // TODO...
    return 0;
}



static u16 x0_scroll = 0;
static u16 y0_scroll = 0;
static u16 x3_scroll = 0;
static u16 y3_scroll = 0;



void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
    switch (layer) {
    case Layer::background:
    case Layer::overlay:
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



void Platform::set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors)
{
    // TODO...
}



TileDesc Platform::get_tile(Layer layer, u16 x, u16 y)
{
    // TODO...
    return 0;
}



void Platform::fill_overlay(u16 TileDesc)
{

}



void Platform::set_overlay_origin(Float x, Float y)
{

}



bool Platform::overlay_texture_exists(const char* name)
{
    return true;
}



void Platform::sleep(Frame frames)
{
    // ...
}



bool Platform::is_running() const
{
    return true;
}



void Platform::feed_watchdog()
{
    // ...
}



void Platform::on_unrecoverrable_error(UnrecoverrableErrorCallback callback)
{
    // ...
}



bool Platform::write_save_data(const void* data, u32 length, u32 offset)
{
    return false;
}



bool Platform::read_save_data(void* buffer, u32 data_length, u32 offset)
{
    return false;
}



int Platform::save_capacity()
{
    return 32000;
}



const char* Platform::load_file_contents(const char* folder,
                                         const char* filename) const
{
    return "";
}


void Platform::stackcheck()
{
    // ...
}



void Platform::walk_filesystem(Function<32, void(const char* path)>)
{
    // ...
}



static int scratch_buffers_in_use = 0;



static ObjectPool<PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>,
                  scratch_buffer_count>
scratch_buffer_pool;



ScratchBufferPtr Platform::make_scratch_buffer()
{
    if (not scratch_buffers_remaining()) {
        // if (scratch_buffer_oom_handler) {
        //     (*scratch_buffer_oom_handler)();

        //     if (not scratch_buffers_remaining()) {
        //         log_data_.reset();
        //     }
        // }
    }

    auto finalizer =
        [](PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>* ctrl) {
            --scratch_buffers_in_use;
            ctrl->pool_->post(ctrl);
        };

    auto maybe_buffer = create_pooled_rc<ScratchBuffer, scratch_buffer_count>(
        &scratch_buffer_pool, finalizer);
    if (maybe_buffer) {
        ++scratch_buffers_in_use;
        return *maybe_buffer;
    } else {
        screen().fade(1.f, ColorConstant::electric_blue);
        fatal("scratch buffer pool exhausted");
    }
}



void Platform::set_scratch_buffer_oom_handler(Function<16, void()> callback)
{
    // ...
}



int Platform::scratch_buffers_remaining()
{
    return scratch_buffer_count - scratch_buffers_in_use;
}



Platform::SystemClock::SystemClock()
{

}



Platform::DeltaClock::~DeltaClock()
{

}



#define REG_TM3CNT_L *(volatile u16*)(0x04000000 + 0x10c)
#define REG_TM3CNT_H *(volatile u16*)(0x04000000 + 0x10e)



static size_t delta_total;



static int delta_read_tics()
{
    return REG_TM3CNT_L + delta_total;
}



static Microseconds delta_convert_tics(int tics)
{
    return ((tics * (59.59f / 60.f)) * 60.f) / 1000.f;
}



Microseconds Platform::DeltaClock::reset()
{
    irqDisable(IRQ_TIMER3);
    const auto tics = delta_read_tics();
    REG_TM3CNT_H = 0;

    irqEnable(IRQ_TIMER3);

    delta_total = 0;

    REG_TM3CNT_L = 0;
    REG_TM3CNT_H = 1 << 7 | 1 << 6;

    return delta_convert_tics(tics);
}



Platform::DeltaClock::TimePoint Platform::DeltaClock::sample() const
{
    return 0;
}



Platform::DeltaClock::DeltaClock()
{
    irqEnable(IRQ_TIMER3);
    irqSet(IRQ_TIMER3, [] {
        delta_total += 0xffff;

        REG_TM3CNT_H = 0;
        REG_TM3CNT_L = 0;
        REG_TM3CNT_H = 1 << 7 | 1 << 6;
    });
}



void Platform::Screen::draw(const Sprite& spr)
{
    // ...
}



void Platform::Screen::clear()
{
    swiWaitForVBlank();

    auto view_offset = view_.get_center().cast<s32>();
    REG_BG0HOFS = x0_scroll + view_offset.x;
    REG_BG0VOFS = y0_scroll + view_offset.y;

    REG_BG3HOFS = x3_scroll + view_offset.x;
    REG_BG3VOFS = y3_scroll + view_offset.y;
}



void Platform::Screen::display()
{

}



Vec2<u32> Platform::Screen::size() const
{
    return {256, 192};
}



void Platform::Screen::set_contrast(Contrast contrast)
{

}



Contrast Platform::Screen::get_contrast() const
{
    return 1;
}



void Platform::Screen::set_shader(Shader shader)
{

}


void Platform::Screen::set_shader_argument(int value)
{

}


void Platform::Screen::fade(float amount,
                            ColorConstant color,
                            std::optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{

}



void Platform::Screen::schedule_fade(Float amount,
                                     ColorConstant color,
                                     bool include_sprites,
                                     bool include_overlay)
{

}



void Platform::Screen::pixelate(u8 amount,
                                bool include_overlay,
                                bool include_background,
                                bool include_sprites)
{

}



////////////////////////////////////////////////////////////////////////////////
//
// I borrowed the diagram below from the GBA edition of SKYLAND. There's no
// significant difference on the NDS in 2D mode.
//
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



Platform::Screen::Screen()
{
    // clang-format off

    REG_DISPCNT = MODE_0_2D
        | DISPLAY_SPR_1D_LAYOUT
        | DISPLAY_BG0_ACTIVE
        | DISPLAY_BG1_ACTIVE
        //| DISPLAY_BG2_ACTIVE
        //| DISPLAY_BG3_ACTIVE
        | DISPLAY_SPR_ACTIVE;

    bgInit(0, BgType_Text4bpp, BgSize_T_512x256, sbb_t0_tiles, cbb_t0_texture);
    bgInit(3, BgType_Text4bpp, BgSize_T_512x256, sbb_t1_tiles, cbb_t1_texture);
    bgInit(1, BgType_Text4bpp, BgSize_T_256x256, sbb_bg_tiles, cbb_background_texture);
    // bgInit(2, BgType_Text4bpp, BgSize_T_256x256, sbb_overlay_tiles, cbb_overlay_texture);

    View view;
    view.set_size(size().cast<Float>());
    set_view(view);

    // clang-format on
}



ColorConstant passthrough_shader(int palette, ColorConstant k, int arg)
{
    return k;
}



ColorConstant grayscale_shader(int palette, ColorConstant k, int arg)
{
    return k;
}



ColorConstant contrast_shader(int palette, ColorConstant k, int arg)
{
    return k;
}



void Platform::load_sprite_texture(const char* name)
{

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


    ((u16*)SCREEN_BASE_BLOCK(screen_block))[0 + ref(x % 16, y)] =
        (tile_id * 4 + 0) | TILE_PALETTE(palette);

    ((u16*)SCREEN_BASE_BLOCK(screen_block))[1 + ref(x % 16, y)] =
        (tile_id * 4 + 1) | TILE_PALETTE(palette);

    ((u16*)SCREEN_BASE_BLOCK(screen_block))[0 + ref(x % 16, y) + 32] =
        (tile_id * 4 + 2) | TILE_PALETTE(palette);

    ((u16*)SCREEN_BASE_BLOCK(screen_block))[1 + ref(x % 16, y) + 32] =
        (tile_id * 4 + 3) | TILE_PALETTE(palette);
}



void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
    if (layer == Layer::map_1) {
        ((u16*)SCREEN_BASE_BLOCK(sbb_t1_tiles))[x + y * 32] = val | TILE_PALETTE(2);
    } else if (layer == Layer::map_0) {
        ((u16*)SCREEN_BASE_BLOCK(sbb_t0_tiles))[x + y * 32] = val | TILE_PALETTE(0);
    }
}



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        TileDesc val,
                        std::optional<u16> palette)
{
    switch (layer) {
    case Layer::overlay:
        if (x > 31 or y > 31) {
            return;
        }
        bgGetMapPtr(1)[x + y * 32] = val | TILE_PALETTE(1);
        break;

    case Layer::map_0_ext:
        set_map_tile_16p(sbb_t0_tiles, x, y, val, palette ? *palette : 0);
        break;

    case Layer::map_1_ext:
        set_map_tile_16p(sbb_t1_tiles, x, y, val, palette ? *palette : 3);
        break;

    default:
        // TODO...
        break;
    }
}



// FIXME: find a faster memcpy. I'm new to nds dev. On gba, I had a fast
// function for this, but it's written in arm7 assembly.
void slow_memcpy_16(u16* dest, u16* src, int halfwords)
{
    while (halfwords) {
        *(dest++) = *(src++);
        --halfwords;
    }
}



void Platform::load_tile0_texture(const char* name)
{
    for (auto& info : tile_textures) {

        if (str_cmp(name, info.name_) == 0) {

            for (int i = 0; i < 16; ++i) {
                BG_PALETTE[i] = info.palette_data_[i];
            }

            slow_memcpy_16(bgGetGfxPtr(0),
                           (u16*)info.tile_data_,
                           info.tile_data_length_ / 2);

            return;
        }
    }
}



void Platform::load_tile1_texture(const char* name)
{
    for (auto& info : tile_textures) {

        if (str_cmp(name, info.name_) == 0) {

            for (int i = 0; i < 16; ++i) {
                BG_PALETTE[32 + i] = info.palette_data_[i];
            }

            slow_memcpy_16(bgGetGfxPtr(3),
                           (u16*)info.tile_data_,
                           info.tile_data_length_ / 2);

            return;
        }
    }
}



bool Platform::load_overlay_texture(const char* name)
{
    for (auto& info : tile_textures) {

        if (str_cmp(name, info.name_) == 0) {

            for (int i = 0; i < 16; ++i) {
                BG_PALETTE[16 + i] = info.palette_data_[i];
            }

            slow_memcpy_16(bgGetGfxPtr(1),
                           // (u16*)SCREEN_BASE_BLOCK(sbb_overlay_texture),
                           (u16*)info.tile_data_,
                           std::min((size_t)info.tile_data_length_ / 2,
                                    (size_t)0x4000 / 2));
            return true;
        }
    }

    return true;
}



void Platform::load_background_texture(const char* name)
{

}



////////////////////////////////////////////////////////////////////////////////
//
// Keyboard
//
////////////////////////////////////////////////////////////////////////////////



#define KEYS_CUR (((~REG_KEYINPUT)&0x3ff))



void Platform::Keyboard::poll()
{
    std::copy(std::begin(states_), std::end(states_), std::begin(prev_));

    u16 keys = KEYS_CUR;

    states_[(int)Key::start] = keys & KEY_START;
    states_[(int)Key::select] = keys & KEY_SELECT;
    states_[(int)Key::left] = keys & KEY_LEFT;
    states_[(int)Key::right] = keys & KEY_RIGHT;
    states_[(int)Key::up] = keys & KEY_UP;
    states_[(int)Key::down] = keys & KEY_DOWN;
    states_[(int)Key::alt_1] = keys & KEY_L;
    states_[(int)Key::alt_2] = keys & KEY_R;
    states_[(int)Key::action_1] = keys & KEY_A;
    states_[(int)Key::action_2] = keys & KEY_B;
}



void Platform::Keyboard::rumble(bool enabled)
{
    // TODO: rumble pack?
}



////////////////////////////////////////////////////////////////////////////////
//
// Logger
//
////////////////////////////////////////////////////////////////////////////////


void Platform::Logger::log(Severity severity, const char* msg)
{
    iprintf("%s\n", msg);
}



void Platform::Logger::flush()
{

}



Vector<char>* Platform::Logger::data()
{
    return nullptr;
}



void Platform::Logger::set_threshold(Severity severity)
{

}



Platform::Logger::Logger()
{

}



////////////////////////////////////////////////////////////////////////////////
//
// Speaker
//
////////////////////////////////////////////////////////////////////////////////


void Platform::Speaker::play_music(const char* name, Microseconds offset)
{

}



void Platform::Speaker::stop_music()
{

}



void Platform::Speaker::set_music_volume(u8 volume)
{

}



bool Platform::Speaker::is_music_playing(const char* name)
{
    return true;
}



void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   std::optional<Vec2<Float>> position)
{

}



bool Platform::Speaker::is_sound_playing(const char* name)
{
    return true;
}



Platform::Speaker::Speaker()
{

}



void Platform::Speaker::clear_sounds()
{

}



void Platform::Speaker::set_position(const Vec2<Float>& position)
{

}



Microseconds Platform::Speaker::track_length(const char* sound_or_music_name)
{
    return 1;
}



////////////////////////////////////////////////////////////////////////////////
//
// NetworkPeer
//
////////////////////////////////////////////////////////////////////////////////


Platform::NetworkPeer::NetworkPeer()
{

}



Platform::NetworkPeer::~NetworkPeer()
{

}



void Platform::NetworkPeer::listen()
{

}



void Platform::NetworkPeer::disconnect()
{

}



bool Platform::NetworkPeer::is_connected() const
{
    return false;
}



bool Platform::NetworkPeer::is_host() const
{
    return false;
}



Platform::NetworkPeer::Interface Platform::NetworkPeer::interface() const
{
    return internet;
}



bool Platform::NetworkPeer::send_message(const Message& message)
{
    return true;
}



void Platform::NetworkPeer::update()
{

}



std::optional<Platform::NetworkPeer::Message> Platform::NetworkPeer::poll_message()
{
    return {};
}



void Platform::NetworkPeer::poll_consume(u32 length)
{

}



bool Platform::NetworkPeer::supported_by_device()
{
    return false;
}



Platform::NetworkPeer::Stats Platform::NetworkPeer::stats()
{
    return {};
}



////////////////////////////////////////////////////////////////////////////////
//
// RemoteConsole
//
////////////////////////////////////////////////////////////////////////////////


std::optional<Platform::RemoteConsole::Line> Platform::RemoteConsole::readline()
{
    return {};
}



bool Platform::RemoteConsole::printline(const char* text, bool show_prompt)
{
    return true;
}



////////////////////////////////////////////////////////////////////////////////
//
// Platform
//
////////////////////////////////////////////////////////////////////////////////



volatile int frame = 0;



void Vblank()
{
    frame++;
}



Platform::Platform()
{
    ::platform = this;

    consoleDemoInit();

    irqSet(IRQ_VBLANK, Vblank);
}



Platform::~Platform()
{
    // ...
}
