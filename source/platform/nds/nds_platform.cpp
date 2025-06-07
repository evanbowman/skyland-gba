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
// Platform Implementation for NintendoDS
//
//
////////////////////////////////////////////////////////////////////////////////


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
//
// Much of the code is also lifted from my own prior implementation for gba
// hardware.
//


#include "/opt/devkitpro/libnds/include/nds.h"
#include "filesystem.hpp"
#include "images.cpp"
#include "platform/color.hpp"
#include "platform/platform.hpp"



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



static bool main_on_top = false;



static const TextureData* current_spritesheet = &sprite_textures[0];
static const TextureData* current_tilesheet0 = &tile_textures[0];
static const TextureData* current_tilesheet1 = &tile_textures[1];
static const TextureData* current_overlay_texture = &overlay_textures[1];
static const TextureData* current_background = &tile_textures[0];



static u16 sprite_palette[16];
static u16 tilesheet_0_palette[16];
static u16 tilesheet_1_palette[16];
static u16 overlay_palette[16];
static u16 background_palette[16];



static u8 last_fade_amt;
static ColorConstant last_color;
static bool last_fade_include_sprites;
static bool overlay_was_faded;



alignas(4) static u16 sp_palette_back_buffer[32];
alignas(4) static u16 bg_palette_back_buffer[256];


static u16 overlay_back_buffer[1024] alignas(u32);
static bool overlay_back_buffer_changed = false;


constexpr u32 oam_count = Platform::Screen::sprite_limit;
static u32 last_oam_write_index = 0;
static u32 oam_write_index = 0;


static ObjectAttributes
    object_attribute_back_buffer[Platform::Screen::sprite_limit];


static auto affine_transform_back_buffer =
    reinterpret_cast<ObjectAffineMatrix*>(object_attribute_back_buffer);


static const u32 affine_transform_limit = 32;
static u32 affine_transform_write_index = 0;
static u32 last_affine_transform_write_index = 0;


static bool save_chip_valid = false;


static Platform::Screen::Shader shader = passthrough_shader;
static int shader_argument = 0;


struct GlyphMapping
{
    u16 mapper_offset_;

    // -1 represents unassigned. Mapping a tile into memory sets the reference
    //  count to zero. When a call to Platform::set_tile reduces the reference
    //  count back to zero, the tile is once again considered to be unassigned,
    //  and will be set to -1.
    s16 reference_count_ = -1;

    bool valid() const
    {
        return reference_count_ > -1;
    }
};


static constexpr const auto glyph_start_offset = 1;
static constexpr const auto glyph_mapping_count = 78;
static constexpr const auto glyph_expanded_count = 160;
static int glyph_table_size = glyph_mapping_count;
static const int font_color_index_tile = 81;
static const int bad_glyph = 111;


struct GlyphTable
{
    GlyphMapping mappings_[glyph_mapping_count + glyph_expanded_count];
};

static GlyphTable glyph_table;



static bool is_glyph(u16 t)
{
    return t >= glyph_start_offset and
           t - glyph_start_offset < glyph_table_size;
}



static constexpr int vram_tile_size()
{
    // 8 x 8 x (4 bitsperpixel / 8 bitsperbyte)
    return 32;
}



static u8* font_index_tile()
{
    return (u8*)bgGetGfxPtr(0) + ((font_color_index_tile)*vram_tile_size());
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



enum class GlobalFlag {
    rtc_faulty,
    gbp_unlocked,
    multiplayer_connected,
    glyph_mode,
    parallax_clouds,
    v_parallax,
    palette_sync,
    count
};


static Bitvector<static_cast<int>(GlobalFlag::count)> gflags;



static void set_gflag(GlobalFlag f, bool val)
{
    gflags.set(static_cast<int>(f), val);
}



static bool get_gflag(GlobalFlag f)
{
    return gflags.get(static_cast<int>(f));
}



void start(Platform&);



static Platform* platform;



int main(int argc, char** argv)
{
    Platform pfrm;

    start();
}



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



Optional<DateTime> Platform::startup_time() const
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
    while (true)
        ;
}


s16 parallax_table[SCREEN_HEIGHT + 1];
s16 vertical_parallax_table[SCREEN_HEIGHT + 1];



//! General DMA transfer macro
#define DMA_TRANSFER(_dst, _src, count, ch, mode)                              \
    do {                                                                       \
        REG_DMA[ch].cnt = 0;                                                   \
        REG_DMA[ch].src = (const void*)(_src);                                 \
        REG_DMA[ch].dst = (void*)(_dst);                                       \
        REG_DMA[ch].cnt = (count) | (mode);                                    \
    } while (0)


typedef struct DMA_REC
{
    const void* src;
    void* dst;
    u32 cnt;
} DMA_REC;


#define REG_BASE 0x04000000
#define REG_DMA                                                                \
    ((volatile DMA_REC*)(REG_BASE + 0x00B0)) //!< DMA as DMA_REC array
// #define DMA_ENABLE		0x80000000	//!< Enable DMA
// #define DMA_REPEAT		0x02000000	//!< Repeat transfer at next start condition
// #define DMA_AT_HBLANK	0x20000000	//!< Start transfer at HBlank
#define DMA_DST_RELOAD                                                         \
    0x00600000 //!< Increment destination, reset after full run
// NOTE: compared to the gba, some dma parameters are in slightly different
// parts of the register!
#define DMA_HDMA                                                               \
    (DMA_ENABLE | DMA_REPEAT | DMA_START_HBL | DMA_DST_INC | DMA_DST_RESET)



static void vblank_full_transfer_scroll_isr()
{
    DMA_TRANSFER(&REG_BG3HOFS, &parallax_table[1], 1, 0, DMA_HDMA);

    DMA_TRANSFER(&REG_BG3VOFS, &vertical_parallax_table[1], 1, 3, DMA_HDMA);
}



static void vblank_horizontal_transfer_scroll_isr()
{
    DMA_TRANSFER(&REG_BG3HOFS, &parallax_table[1], 1, 0, DMA_HDMA);

    DMA_TRANSFER(&REG_BG3VOFS, &vertical_parallax_table[1], 1, 3, 0);
}



void (*vblank_scroll_callback)() = vblank_full_transfer_scroll_isr;



Optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    return {};
}



void Platform::enable_glyph_mode(bool enabled)
{
    if (enabled) {
        for (auto& gm : ::glyph_table.mappings_) {
            gm.reference_count_ = -1;
        }
    }
    set_gflag(GlobalFlag::glyph_mode, enabled);
}



u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    if (layer == Layer::overlay) {
        return (overlay_back_buffer[x + y * 32] & (0xF000)) >> 12;
    }

    fatal("get_palette unimplemented for input layer.");
}



static u16 x1_scroll = 0;
static u16 y1_scroll = 0;
static u16 x2_scroll = 0;
static u16 y2_scroll = 0;



void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
    switch (layer) {
    case Layer::background:
    case Layer::overlay:
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
        x1_scroll = x;
        y1_scroll = y;
        break;

    case Layer::map_1_ext:
    case Layer::map_1:
        x2_scroll = x;
        y2_scroll = y;
        break;
    }
}



using PaletteBank = u8;

static const PaletteBank custom_text_palette_begin = 3;
static const PaletteBank custom_text_palette_end = 9;
static const auto custom_text_palette_count =
    custom_text_palette_end - custom_text_palette_begin;

static PaletteBank custom_text_palette_write_ptr = custom_text_palette_begin;
static const TextureData* custom_text_palette_source_texture = nullptr;



TileDesc Platform::get_tile(Layer layer, u16 x, u16 y)
{
    switch (layer) {

    case Layer::overlay:
        if (x > 31 or y > 31) {
            return 0;
        }
        return overlay_back_buffer[x + y * 32] & ~(0xF000);

    default:
        return 0;
    }
}



void Platform::fill_overlay(u16 tile)
{
    if (get_gflag(GlobalFlag::glyph_mode) and is_glyph(tile)) {
        // This is moderately complicated to implement, better off just not
        // allowing fills for character tiles.
        return;
    }

    const u16 tile_info = tile | TILE_PALETTE(1);
    const u32 fill_word = tile_info | (tile_info << 16);

    u32* const mem = (u32*)overlay_back_buffer;
    overlay_back_buffer_changed = true;

    for (unsigned i = 0; i < 2048 / sizeof(u32); ++i) {
        mem[i] = fill_word;
    }

    if (get_gflag(GlobalFlag::glyph_mode)) {
        for (auto& gm : ::glyph_table.mappings_) {
            gm.reference_count_ = -1;
        }
    }
}



void Platform::set_overlay_origin(Float x, Float y)
{
    REG_BG0HOFS = static_cast<s16>(x);
    REG_BG0VOFS = static_cast<s16>(y);
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
    if (not save_chip_valid) {
        return false;
    }



    return false;
}



bool Platform::read_save_data(void* buffer, u32 data_length, u32 offset)
{
    if (not save_chip_valid) {
        return false;
    }



    return false;
}



int Platform::save_capacity()
{
    info(*platform, format("save cap %", cardEepromGetSize()));
    return 32000;
}



const char* Platform::load_file_contents(const char* folder,
                                         const char* filename) const
{
    StringBuffer<64> path("/");

    if (strlen(folder) > 0) {
        path += folder;
        path += "/";
    }

    path += filename;

    return filesystem::load(path.c_str());
}


void Platform::stackcheck()
{
    // ...
}



void Platform::walk_filesystem(
    Function<8 * sizeof(void*), void(const char* path)> callback)
{
    filesystem::walk(callback);
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
    return (((tics / 2) * (59.59f / 60.f)) * 60.f) / 1000.f;
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
    if (UNLIKELY(spr.get_alpha() == Sprite::Alpha::transparent)) {
        return;
    }

    // const auto& mix = spr.get_mix();


    auto pb = 0; // [&]() -> PaletteBank {
    //     if (UNLIKELY(mix.color_ not_eq ColorConstant::null)) {
    //         if (const auto pal_bank = color_mix(mix.color_, mix.amount_)) {
    //             return pal_bank;
    //         } else {
    //             return 0;
    //         }
    //     } else {
    //         return 0;
    //     }
    // }();

    if (spr.palette()) {
        pb = spr.palette();
    }

    auto draw_sprite = [&](int tex_off, int x_off, int scale) {
        if (UNLIKELY(oam_write_index == oam_count)) {
            return;
        }
        const auto position =
            spr.get_position().cast<s32>() - spr.get_origin().cast<s32>();

        const auto view_center = view_.get_center().cast<s32>();
        auto oa = object_attribute_back_buffer + oam_write_index;
        if (spr.get_alpha() not_eq Sprite::Alpha::translucent) {
            oa->attribute_0 = ATTR0_COLOR_16 | ATTR0_TALL;
        } else {
            oa->attribute_0 = ATTR0_COLOR_16 | ATTR0_TALL | ATTR0_TYPE_BLENDED;
        }
        oa->attribute_1 = ATTR1_SIZE_32; // clear attr1

        auto abs_position = position - view_center;

        // oa->attribute_0 &= (0xff00 & ~((1 << 8) | (1 << 9))); // clear attr0

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

                oa->attribute_0 |= ATTR0_ROTSCALE;
                oa->attribute_0 |= ATTR0_ROTSCALE_DOUBLE;

                abs_position.x -= 8;
                abs_position.y -= 16;

                oa->attribute_1 |= ATTR1_ROTDATA(affine_transform_write_index);

                affine_transform_write_index += 1;
            }
        } else {
            const auto& flip = spr.get_flip();
            if (flip.y) {
                oa->attribute_1 |= ATTR1_FLIP_Y;
            }
            if (flip.x) {
                oa->attribute_1 |= ATTR1_FLIP_X;
            }
        }

        oa->attribute_0 |= OBJ_Y(abs_position.y);

        // if (not mix.amount_ and screen_pixelate_amount not_eq 0) {

        //     oa->attribute_0 |= ATTR0_MOSAIC;
        // }

        oa->attribute_1 |= OBJ_X(abs_position.x + x_off);

        auto ti = spr.get_texture_index();

        const auto target_index = 2 + ti * scale + tex_off;
        oa->attribute_2 = target_index;
        oa->attribute_2 |= ATTR2_PALETTE(pb);
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
            draw_sprite(0, 0, 16);
            draw_sprite(8, 16, 16);

        } else {
            draw_sprite(0, 16, 16);
            draw_sprite(8, 0, 16);
        }

        break;

    case Sprite::Size::w16_h32:
        draw_sprite(0, 0, 8);
        break;
    }
}



void memcpy32(u32* dest, u32* src, int word_count)
{
    while (word_count--) {
        *(dest++) = *(src++);
    }
}



// FIXME: find a faster memcpy. I'm new to nds dev. On gba, I had a fast
// function for this, but it's written in arm7 assembly.
void memcpy16(u16* dest, u16* src, int halfwords)
{
    while (halfwords) {
        *(dest++) = *(src++);
        --halfwords;
    }
}



void Platform::Screen::clear()
{
    touchPosition touch;

    touchRead(&touch);

    touch_.previous_ = touch_.current_;

    const auto keys = keysHeld();
    if (keys & KEY_TOUCH) {
        touch_.current_ = {touch.px, touch.py};
    } else {
        touch_.current_.reset();
    }

    swiWaitForVBlank();

    auto view_offset = view_.get_center().cast<s32>();
    REG_BG1HOFS = x1_scroll + view_offset.x;
    REG_BG1VOFS = y1_scroll + view_offset.y;

    REG_BG2HOFS = x2_scroll + view_offset.x;
    REG_BG2VOFS = y2_scroll + view_offset.y;

    if (get_gflag(GlobalFlag::palette_sync)) {


        memcpy16((u16*)SPRITE_PALETTE, (u16*)sp_palette_back_buffer, 32);

        memcpy16((u16*)BG_PALETTE, (u16*)bg_palette_back_buffer, 48);

        memcpy16(
            (u16*)(BG_PALETTE + 16 * 11),
            (u16*)bg_palette_back_buffer + 16 * 11,
            32); // 16 words, both the background palette and the flag palette.

        set_gflag(GlobalFlag::palette_sync, false);
    }
}



void Platform::Screen::display()
{
    if (overlay_back_buffer_changed) {
        overlay_back_buffer_changed = false;

        memcpy16(bgGetMapPtr(0),
                 (u16*)overlay_back_buffer,
                 (sizeof overlay_back_buffer) / 2);
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

    memcpy16(OAM,
             (u16*)object_attribute_back_buffer,
             (sizeof object_attribute_back_buffer) / 2);


    last_affine_transform_write_index = affine_transform_write_index;
    affine_transform_write_index = 0;

    last_oam_write_index = oam_write_index;
    oam_write_index = 0;
}



const Platform::Screen::Touch* Platform::Screen::touch() const
{
    if (not main_on_top) {
        return &touch_;
    } else {
        return nullptr;
    }
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



ColorConstant grayscale_shader(int palette, ColorConstant k, int arg)
{
    return Color::from_bgr_hex_555(
               blend(Color(k), Color(k).grayscale(), (u8)arg))
        .hex();
}



ColorConstant contrast_shader(int palette, ColorConstant k, int arg)
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



ColorConstant passthrough_shader(int palette, ColorConstant k, int arg)
{
    return k;
}



void Platform::Screen::set_shader(Shader shader)
{
    ::shader = shader;
    set_shader_argument(0);
}



static Color invoke_shader(const Color& c, int palette)
{
    return shader(palette, c.hex(), shader_argument);
}



static void init_palette(const TextureData* td, u16* palette, int palette_id)
{
    for (int i = 0; i < 16; ++i) {
        palette[i] =
            invoke_shader(Color::from_bgr_hex_555(td->palette_data_[i]),
                          palette_id)
                .bgr_hex_555();
    }
}



void Platform::Screen::set_shader_argument(int arg)
{
    shader_argument = arg;

    init_palette(current_spritesheet, sprite_palette, 16);
    init_palette(current_tilesheet0, tilesheet_0_palette, 0);
    init_palette(current_tilesheet1, tilesheet_1_palette, 2);
    init_palette(current_background, background_palette, 11);
}



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

    const auto c = invoke_shader(Color(k), 0);

    if (not base) {
        // Sprite palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(sprite_palette[i]);
            SPRITE_PALETTE[i] = blend(from, c, include_sprites ? amt : 0);
        }
        // Tile0 palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(tilesheet_0_palette[i]);
            BG_PALETTE[i] = blend(from, c, amt);
        }
        // Custom flag palette?
        for (int i = 0; i < 16; ++i) {
            auto from =
                Color::from_bgr_hex_555(tile_textures[0].palette_data_[i]);
            BG_PALETTE[16 * 12 + i] = blend(from, c, amt);
        }
        // Tile1 palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(tilesheet_1_palette[i]);
            BG_PALETTE[32 + i] = blend(from, c, amt);
        }
        // Custom flag palette
        for (int i = 0; i < 16; ++i) {
            auto from = Color::from_bgr_hex_555(background_palette[i]);
            BG_PALETTE[16 * 11 + i] = blend(from, c, amt);
        }
        // Overlay palette
        if (include_overlay or overlay_was_faded) {
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(overlay_palette[i]);
                BG_PALETTE[16 + i] = blend(from, c, include_overlay ? amt : 0);
            }
        }
        overlay_was_faded = include_overlay;
    } else {
        const auto bc = invoke_shader(Color(*base), 0);
        for (int i = 0; i < 16; ++i) {
            SPRITE_PALETTE[i] = blend(bc, c, include_sprites ? amt : 0);
            BG_PALETTE[i] = blend(bc, c, amt);
            BG_PALETTE[32 + i] = blend(bc, c, amt);

            if (overlay_was_faded) {
                // FIXME!
                for (int i = 0; i < 16; ++i) {
                    auto from = Color::from_bgr_hex_555(overlay_palette[i]);
                    BG_PALETTE[16 + i] = blend(from, c, 0);
                }
                overlay_was_faded = false;
            }
        }
    }
}



void Platform::Screen::schedule_fade(Float amount,
                                     ColorConstant k,
                                     bool include_sprites,
                                     bool include_overlay)
{
    const u8 amt = amount * 255;

    if (amt == last_fade_amt and k == last_color and
        last_fade_include_sprites == include_sprites) {
        return;
    }

    last_fade_amt = amt;
    last_color = k;
    last_fade_include_sprites = include_sprites;

    const auto c = invoke_shader(Color(k), 0);


    set_gflag(GlobalFlag::palette_sync, true);


    // Sprite palette
    for (int i = 0; i < 16; ++i) {
        auto from = Color::from_bgr_hex_555(sprite_palette[i]);
        sp_palette_back_buffer[i] = blend(from, c, include_sprites ? amt : 0);
    }
    // Tile0 palette
    for (int i = 0; i < 16; ++i) {
        auto from = Color::from_bgr_hex_555(tilesheet_0_palette[i]);
        bg_palette_back_buffer[i] = blend(from, c, amt);
    }
    // Custom flag/tile/sprite palette:
    for (int i = 0; i < 16; ++i) {
        auto from = Color::from_bgr_hex_555(tile_textures[0].palette_data_[i]);
        auto val = blend(from, c, amt);
        bg_palette_back_buffer[16 * 12 + i] = val;
        sp_palette_back_buffer[16 + i] = val;
    }
    // Tile1 palette
    for (int i = 0; i < 16; ++i) {
        auto from = Color::from_bgr_hex_555(tilesheet_1_palette[i]);
        bg_palette_back_buffer[32 + i] = blend(from, c, amt);
    }
    for (int i = 0; i < 16; ++i) {
        auto from = Color::from_bgr_hex_555(background_palette[i]);
        bg_palette_back_buffer[16 * 11 + i] = blend(from, c, amt);
    }
    // Overlay palette
    for (int i = 0; i < 16; ++i) {
        auto from = Color::from_bgr_hex_555(overlay_palette[i]);
        bg_palette_back_buffer[16 + i] =
            blend(from, c, include_overlay ? amt : 0);
    }
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
        | SpriteMapping_1D_32
        | DISPLAY_BG0_ACTIVE
        | DISPLAY_BG1_ACTIVE
        | DISPLAY_BG2_ACTIVE
        | DISPLAY_BG3_ACTIVE
        | DISPLAY_SPR_ACTIVE;

    bgInit(0, BgType_Text4bpp, BgSize_T_256x256, sbb_overlay_tiles, cbb_overlay_texture);
    bgInit(1, BgType_Text4bpp, BgSize_T_512x256, sbb_t0_tiles, cbb_t0_texture);
    bgInit(2, BgType_Text4bpp, BgSize_T_512x256, sbb_t1_tiles, cbb_t1_texture);
    bgInit(3, BgType_Text4bpp, BgSize_T_256x256, sbb_bg_tiles, cbb_background_texture);

    bgSetPriority(0, 0);
    bgSetPriority(1, 2);
    bgSetPriority(2, 2);
    bgSetPriority(3, 3);


    View view;
    view.set_size(size().cast<Float>());
    set_view(view);

    // clang-format on

    lcdMainOnBottom();

    for (u32 i = 0; i < Screen::sprite_limit; ++i) {
        object_attribute_back_buffer[i].attribute_2 = ATTR2_PRIORITY(3);
        object_attribute_back_buffer[i].attribute_0 |= ATTR0_DISABLED;
    }

#define BLD_BUILD(top, bot, mode)                                              \
    ((((bot) & 63) << 8) | (((mode) & 3) << 6) | ((top) & 63))
#define BLD_OBJ 0x0010
#define BLD_BG0 0x0001
#define BLD_BG1 0x0002
#define BLD_BG3 0x0008
#define BLDA_BUILD(eva, evb) (((eva) & 31) | (((evb) & 31) << 8))

    REG_BLDCNT = BLD_BUILD(BLD_OBJ, BLD_BG0 | BLD_BG1 | BLD_BG3, 0);
    REG_BLDALPHA = BLDA_BUILD(0x40 / 8, 0x40 / 8);
}



void Platform::load_sprite_texture(const char* name)
{
    for (auto& info : sprite_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_spritesheet = &info;

            init_palette(current_spritesheet, sprite_palette, 16);

            // NOTE: for some reason, the sprite tiles were mis-aligned when
            // copying directly to SPRITE_GFX. I'll have to figure this one
            // out... eventually... (p.s. vram_tile_size() is in bytes, so we're
            // skipping two tiles).
            memcpy16(SPRITE_GFX + vram_tile_size(),
                     (u16*)info.tile_data_,
                     std::min((u32)16128, info.tile_data_length_ / 2) -
                         vram_tile_size());

            // We need to do this, otherwise whatever screen fade is currently
            // active will be overwritten by the copy.
            const auto c = invoke_shader(Color(last_color), 16);
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(sprite_palette[i]);
                SPRITE_PALETTE[i] = blend(from, c, last_fade_amt);
            }

            return;
        }
    }

    Platform::fatal("missing sprite texture!");
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

    val = ((u16*)SCREEN_BASE_BLOCK(screen_block))[0 + ref(x % 16, y)];
    val &= ~(0xF000);
    val |= TILE_PALETTE(palette);
    ((u16*)SCREEN_BASE_BLOCK(screen_block))[0 + ref(x % 16, y)] = val;


    val = ((u16*)SCREEN_BASE_BLOCK(screen_block))[1 + ref(x % 16, y)];
    val &= ~(0xF000);
    val |= TILE_PALETTE(palette);
    ((u16*)SCREEN_BASE_BLOCK(screen_block))[1 + ref(x % 16, y)] = val;


    val = ((u16*)SCREEN_BASE_BLOCK(screen_block))[0 + ref(x % 16, y) + 32];
    val &= ~(0xF000);
    val |= TILE_PALETTE(palette);
    ((u16*)SCREEN_BASE_BLOCK(screen_block))[0 + ref(x % 16, y) + 32] = val;


    val = ((u16*)SCREEN_BASE_BLOCK(screen_block))[1 + ref(x % 16, y) + 32];
    val &= ~(0xF000);
    val |= TILE_PALETTE(palette);
    ((u16*)SCREEN_BASE_BLOCK(screen_block))[1 + ref(x % 16, y) + 32] = val;
}



void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
    if (layer == Layer::map_1) {
        ((u16*)SCREEN_BASE_BLOCK(sbb_t1_tiles))[x + y * 32] =
            val | TILE_PALETTE(2);
    } else if (layer == Layer::map_0) {
        ((u16*)SCREEN_BASE_BLOCK(sbb_t0_tiles))[x + y * 32] =
            val | TILE_PALETTE(0);
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
                if (gm.valid()) {
                    gm.reference_count_ -= 1;

                    if (gm.reference_count_ == 0) {
                        gm.reference_count_ = -1;
                        gm.mapper_offset_ = 0;
                    }
                } else {
                    // I suppose this could happen if we swapped overlay
                    // textures, without clearing out existing tiles? Not
                    // sure. The code has been stable in the real world for
                    // quite a while, so I'm commenting out the log line.
                    //
                    // error(
                    //       "existing tile is a glyph, but has no"
                    //       " mapping table entry!");
                }
            }

            if (is_glyph(val)) {
                auto& gm = ::glyph_table.mappings_[val - glyph_start_offset];
                if (not gm.valid()) {
                    // Not clear exactly what to do here... Somehow we've
                    // gotten into an erroneous state, but not a permanently
                    // unrecoverable state (tile isn't valid, so it'll be
                    // overwritten upon the next call to map_tile).
                    warning("invalid assignment to glyph table");
                    return;
                }
                gm.reference_count_++;
            }
        }
    }

    overlay_back_buffer[x + y * 32] = val | TILE_PALETTE(palette);
    overlay_back_buffer_changed = true;
}



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        TileDesc val,
                        Optional<u16> palette)
{
    switch (layer) {
    case Layer::overlay:
        if (x > 31 or y > 31) {
            return;
        }
        set_overlay_tile(*this, x, y, val, 1);
        break;

    case Layer::map_0_ext:
        set_map_tile_16p(sbb_t0_tiles, x, y, val, palette ? *palette : 0);
        break;

    case Layer::map_1_ext:
        set_map_tile_16p(sbb_t1_tiles, x, y, val, 2);
        break;

    case Layer::background:
        if (x > 31 or y > 32) {
            return;
        }
        bgGetMapPtr(3)[x + y * 32] = val | TILE_PALETTE(11);
        break;

    default:
        // TODO...
        break;
    }
}



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
                BG_PALETTE[p * 16 + i] = 0;
            }
        }

        custom_text_palette_source_texture = current_overlay_texture;
    }

    const auto default_colors = font_color_indices();

    const auto fg_color_hash =
        invoke_shader(Color(colors.foreground_), 1).bgr_hex_555();

    const auto bg_color_hash =
        invoke_shader(Color(colors.background_), 1).bgr_hex_555();

    auto existing_mapping = [&]() -> Optional<PaletteBank> {
        for (auto i = custom_text_palette_begin; i < custom_text_palette_end;
             ++i) {
            if (BG_PALETTE[i * 16 + default_colors.fg_] == fg_color_hash and
                BG_PALETTE[i * 16 + default_colors.bg_] == bg_color_hash) {

                return i;
            }
        }
        return {};
    }();

    if (existing_mapping) {
        set_overlay_tile(*this, x, y, glyph, *existing_mapping);
    } else {
        const auto target = custom_text_palette_write_ptr;

        BG_PALETTE[target * 16 + default_colors.fg_] = fg_color_hash;
        BG_PALETTE[target * 16 + default_colors.bg_] = bg_color_hash;

        set_overlay_tile(*this, x, y, glyph, target);

        custom_text_palette_write_ptr =
            ((target + 1) - custom_text_palette_begin) %
                custom_text_palette_count +
            custom_text_palette_begin;

        if (custom_text_palette_write_ptr == custom_text_palette_begin) {
            warning(*this, "wraparound in custom text palette alloc");
        }
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
        set_overlay_tile(*this, x, y, t, palette);
    }
}


void Platform::load_tile0_texture(const char* name)
{
    for (auto& info : tile_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_tilesheet0 = &info;

            init_palette(current_tilesheet0, tilesheet_0_palette, 0);


            // We don't want to load the whole palette into memory, we might
            // overwrite palettes used by someone else, e.g. the overlay...
            //
            // Also, like the sprite texture, we need to apply the currently
            // active screen fade while modifying the color palette.
            const auto c = invoke_shader(Color(last_color), 0);
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(tilesheet_0_palette[i]);
                BG_PALETTE[i] = blend(from, c, last_fade_amt);
            }

            memcpy16(bgGetGfxPtr(1),
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

            current_tilesheet1 = &info;

            init_palette(current_tilesheet1, tilesheet_1_palette, 2);


            // We don't want to load the whole palette into memory, we might
            // overwrite palettes used by someone else, e.g. the overlay...
            //
            // Also, like the sprite texture, we need to apply the currently
            // active screen fade while modifying the color palette.
            const auto c = invoke_shader(Color(last_color), 2);
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(tilesheet_1_palette[i]);
                BG_PALETTE[32 + i] = blend(from, c, last_fade_amt);
            }

            memcpy16(bgGetGfxPtr(2),
                     (u16*)info.tile_data_,
                     info.tile_data_length_ / 2);

            return;
        }
    }
}



void Platform::load_overlay_chunk(TileDesc dst, TileDesc src, u16 count)
{
    const u8* image_data = (const u8*)current_overlay_texture->tile_data_;
    u8* overlay_vram_base_addr = (u8*)bgGetGfxPtr(0);

    const auto chunk_size = vram_tile_size() * count;

    memcpy16((u16*)(overlay_vram_base_addr + vram_tile_size() * dst),
             (u16*)(image_data + vram_tile_size() * src),
             chunk_size / 2);
}



bool Platform::load_overlay_texture(const char* name)
{
    for (auto& info : overlay_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_overlay_texture = &info;

            init_palette(current_overlay_texture, overlay_palette, 1);

            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(overlay_palette[i]);
                if (not overlay_was_faded) {
                    BG_PALETTE[16 + i] = from.bgr_hex_555();
                } else {
                    const auto c = invoke_shader(Color(last_color), 1);
                    BG_PALETTE[16 + i] = blend(from, c, last_fade_amt);
                }
            }

            memcpy16(bgGetGfxPtr(0),
                     (u16*)info.tile_data_,
                     std::min((size_t)info.tile_data_length_ / 2,
                              (size_t)0x4000 / 2));


            if (get_gflag(GlobalFlag::glyph_mode)) {
                for (auto& gm : ::glyph_table.mappings_) {
                    gm.reference_count_ = -1;
                }
            }

            return true;
        }
    }

    return true;
}



void Platform::load_background_texture(const char* name)
{
    for (auto& info : background_textures) {

        if (str_cmp(name, info.name_) == 0) {

            current_background = &info;

            init_palette(current_background, background_palette, 11);

            const auto c = invoke_shader(Color(last_color), 11);
            for (int i = 0; i < 16; ++i) {
                auto from = Color::from_bgr_hex_555(background_palette[i]);
                BG_PALETTE[16 * 11 + i] = blend(from, c, last_fade_amt);
            }

            memcpy16(bgGetGfxPtr(3),
                     (u16*)info.tile_data_,
                     info.tile_data_length_ / 2);
            return;
        }
    }
}



TileDesc Platform::map_glyph(const utf8::Codepoint& glyph,
                             const TextureMapping& mapping_info)
{
    if (not get_gflag(GlobalFlag::glyph_mode)) {
        return bad_glyph;
    }

    for (TileDesc tile = 0; tile < glyph_table_size; ++tile) {
        auto& gm = ::glyph_table.mappings_[tile];
        if (gm.valid() and gm.mapper_offset_ == mapping_info.offset_) {
            return glyph_start_offset + tile;
        }
    }

    for (auto& info : overlay_textures) {
        if (str_cmp(mapping_info.texture_name_, info.name_) == 0) {
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
                if (not gm.valid()) {
                    gm.mapper_offset_ = mapping_info.offset_;
                    gm.reference_count_ = 0;

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

                    alignas(2) u8 buffer[tile_size] = {0};
                    memcpy16((u16*)buffer,
                             (u16*)(info.tile_data_ +
                                    ((u32)mapping_info.offset_ * tile_size) /
                                        sizeof(decltype(info.tile_data_))),
                             tile_size / 2);

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
                    memcpy16((u16*)((u8*)bgGetGfxPtr(0) +
                                    ((t + glyph_start_offset) * tile_size)),
                             (u16*)buffer,
                             tile_size / 2);

                    return t + glyph_start_offset;
                }
            }
        }
    }
    return bad_glyph;
}



////////////////////////////////////////////////////////////////////////////////
//
// Keyboard
//
////////////////////////////////////////////////////////////////////////////////



void Platform::Keyboard::poll()
{
    std::copy(std::begin(states_), std::end(states_), std::begin(prev_));

    const u16 keys = keysHeld();

    scanKeys();

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
    states_[(int)Key::action_4] = keys & KEY_X;
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
                                   Optional<Vec2<Float>> position)
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



Optional<Platform::NetworkPeer::Message> Platform::NetworkPeer::poll_message()
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



////////////////////////////////////////////////////////////////////////////////
//
// RemoteConsole
//
////////////////////////////////////////////////////////////////////////////////


Optional<Platform::RemoteConsole::Line> Platform::RemoteConsole::readline()
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



void vblank_isr()
{
    vblank_scroll_callback();
}



Platform::Platform()
{
    ::platform = this;


    for (int i = 0; i < 16; ++i) {
        BG_PALETTE[(15 * 16) + i] = Color(custom_color(0xef0d54)).bgr_hex_555();
    }
    for (int i = 0; i < 16; ++i) {
        BG_PALETTE[(14 * 16) + i] = Color(custom_color(0x103163)).bgr_hex_555();
    }
    for (int i = 0; i < 16; ++i) {
        BG_PALETTE[(13 * 16) + i] =
            Color(ColorConstant::silver_white).bgr_hex_555();
    }

    load_tile0_texture("tilesheet");
    for (int i = 0; i < 16; ++i) {
        BG_PALETTE[(12 * 16) + i] = BG_PALETTE[i];

        // When we started allowing players to design custom sprites, we needed
        // to reserve a sprite palette and fill it with the same color values as
        // the image editor uses for custom tile graphics.
        SPRITE_PALETTE[16 + i] = BG_PALETTE[i];
    }

    consoleDemoInit();

    sysSetBusOwners(true, true);

    switch (cardEepromGetType()) {
    case -1: // Error or no EEPROM
    case 0:  // Unknown
    case 1:  // 512 Bytes of EEPROM
        save_chip_valid = false;
        break;

    case 3:

        break;
    }

    info(*this, format("save type %", cardEepromGetType()));
    info(*this, format("save size %", cardEepromGetSize()));


    if (not filesystem::is_mounted()) {
        fatal("failed to mount filesystem!");
    }

    irqSet(IRQ_VBLANK, vblank_isr);
}



Platform::~Platform()
{
    // ...
}
