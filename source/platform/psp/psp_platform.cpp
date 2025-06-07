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
// Platform Implementation for Sony PlayStation Portable
//
//
////////////////////////////////////////////////////////////////////////////////


#include "graphics/overlay.hpp"
#include "platform/platform.hpp"
#include <chrono>
#include <memory>
#include <png.h>
#include <pspaudio.h>
#include <pspaudiolib.h>
#include <pspctrl.h>
#include <pspdebug.h>
#include <pspdisplay.h>
#include <pspgu.h>
#include <pspkernel.h>
#include <psprtc.h>
#include <psputility.h>
#include <stdio.h>


extern "C" {
#include "glib2d.h"
}


PSP_MODULE_INFO("SKYLAND", 0, 1, 1);
PSP_MAIN_THREAD_ATTR(THREAD_ATTR_USER | THREAD_ATTR_VFPU);


void start(Platform& pf);


Platform* platform;


int main(int argc, char** argv)
{
    Platform pf;
    ::platform = &pf;

    start(pf);

    return 0;
}


static bool is_running = true;


int exit_callback(int arg1, int arg2, void* common)
{
    sceKernelExitGame();
    is_running = false;
    return 0;
}


int callback_thread(SceSize args, void* argp)
{
    int cbid = sceKernelCreateCallback("Exit Callback", exit_callback, NULL);
    sceKernelRegisterExitCallback(cbid);
    sceKernelSleepThreadCB();

    return 0;
}


int setup_callbacks(void)
{
    int thid = sceKernelCreateThread(
        "update_thread", callback_thread, 0x11, 0xFA0, 0, 0);
    if (thid >= 0) {
        sceKernelStartThread(thid, 0, 0);
    }
    return thid;
}


bool Platform::is_running() const
{
    return ::is_running;
}


Platform::Platform()
{
    setup_callbacks();

    sceCtrlSetSamplingCycle(0);
    sceCtrlSetSamplingMode(PSP_CTRL_MODE_ANALOG);
}


Platform::~Platform()
{
}



void Platform::DynamicTexture::remap(u16 spritesheet_offset)
{
}


Optional<DateTime> Platform::SystemClock::initial_time()
{
    return {};
}


Optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    return {};
}


void Platform::load_sprite_texture(const char* name)
{
}


void Platform::load_tile0_texture(const char* name)
{
}


void Platform::load_tile1_texture(const char* name)
{
}


static bool glyph_mode;


void Platform::enable_glyph_mode(bool enabled)
{
    glyph_mode = enabled;
}


static g2dColor default_text_foreground_color;
static g2dColor default_text_background_color;



bool Platform::load_overlay_texture(const char* name)
{
    return false;
}


static u8 map0_tiles[16][20];
static u8 map1_tiles[16][20];
static u16 background_tiles[32][32];
static u16 overlay_tiles[32][32];


void Platform::fill_overlay(u16 TileDesc)
{
}


static void set_overlay_tile(int x, int y, u16 val)
{
}


void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        u16 val,
                        Optional<u16> palette)
{
}


u16 Platform::get_tile(Layer layer, u16 x, u16 y)
{
    return 0;
}



void Platform::set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors)
{
}


static Vec2<Float> overlay_origin;


void Platform::set_overlay_origin(Float x, Float y)
{
    overlay_origin = {x, y};
}


Platform::DeviceName Platform::device_name() const
{
    return "SonyPSP";
}


void Platform::fatal(const char* msg)
{
    ::is_running = false;
    sceKernelExitGame();
    while (true)
        ;
}


std::pair<const char*, u32> Platform::load_file(const char* folder,
                                                const char* filename) const
{
    return {nullptr, 0};
}


TileDesc Platform::map_glyph(const utf8::Codepoint& glyph,
                             const TextureMapping& mapping)
{
    return 0;
}


const char* savefile_name = "ms0:/skyland.sav";


bool Platform::read_save_data(void* buffer, u32 length, u32 offset)
{
    int fd;
    if (!(fd = sceIoOpen(savefile_name, PSP_O_RDONLY | PSP_O_CREAT, 0777))) {
        return false;
    }

    const auto success = (sceIoRead(fd, buffer, length) == length);

    sceIoClose(fd);
    return success;
}


bool Platform::write_save_data(const void* data, u32 length, u32 offset)
{
    int fd;
    if (!(fd = sceIoOpen(savefile_name, PSP_O_WRONLY | PSP_O_CREAT, 0777))) {
        return false;
    }

    const auto success = (sceIoWrite(fd, data, length) == length);

    sceIoClose(fd);

    return success;
}


////////////////////////////////////////////////////////////////////////////////
// SystemClock
////////////////////////////////////////////////////////////////////////////////


Platform::SystemClock::SystemClock()
{
}


Optional<DateTime> Platform::SystemClock::now()
{
    return {};
}


////////////////////////////////////////////////////////////////////////////////
// NetworkPeer
////////////////////////////////////////////////////////////////////////////////


Platform::NetworkPeer::NetworkPeer()
{
}


bool Platform::NetworkPeer::is_host() const
{
    // TODO
    return false;
}


void Platform::NetworkPeer::disconnect()
{
    // TODO
}


void Platform::NetworkPeer::connect(const char* peer_address)
{
    // TODO
}


void Platform::NetworkPeer::listen()
{
    // TODO
}


Platform::NetworkPeer::Interface Platform::NetworkPeer::interface() const
{
    return Interface::serial_cable;
}


bool Platform::NetworkPeer::is_connected() const
{
    // TODO
    return false;
}


bool Platform::NetworkPeer::send_message(const Message&)
{
    // TODO
    return true;
}


void Platform::NetworkPeer::update()
{
    // TODO
}


auto Platform::NetworkPeer::poll_message() -> Optional<Message>
{
    // TODO
    return {};
}


void Platform::NetworkPeer::poll_consume(u32 length)
{
    // TODO
}


Platform::NetworkPeer::~NetworkPeer()
{
}


////////////////////////////////////////////////////////////////////////////////
// DeltaClock
////////////////////////////////////////////////////////////////////////////////


static u64 last_clock;
static u32 slept_ticks;


Platform::DeltaClock::DeltaClock()
{
    sceRtcGetCurrentTick(&last_clock);
}


Microseconds Platform::DeltaClock::reset()
{
    const auto prev = ::last_clock;
    sceRtcGetCurrentTick(&::last_clock);

    const auto result = (::last_clock - prev) - slept_ticks;

    ::slept_ticks = 0;

    return result;
}


Platform::DeltaClock::~DeltaClock()
{
}


////////////////////////////////////////////////////////////////////////////////
// Screen
////////////////////////////////////////////////////////////////////////////////


Platform::Screen::Screen()
{
}


Vec2<u32> Platform::Screen::size() const
{
    // We upsample the textures by 2x on the psp, because the pixels are so
    // small.
    static const Vec2<u32> psp_half_widescreen{240, 136};
    return psp_half_widescreen;
}



static Buffer<Sprite, 128> sprite_queue;



void Platform::Screen::clear()
{
    sprite_queue.clear();
    g2dClear(BLUE);
}



void Platform::Screen::draw(const Sprite& spr)
{
    sprite_queue.push_back(spr);
}


static g2dColor make_color(int color_hex, u8 alpha)
{
    const auto r = (color_hex & 0xFF0000) >> 16;
    const auto g = (color_hex & 0x00FF00) >> 8;
    const auto b = (color_hex & 0x0000FF);

    return G2D_RGBA(r, g, b, alpha);
}


static bool fade_include_sprites;
static bool fade_include_overlay;
static float fade_amount;
static g2dColor fade_color;

const int fade_rect_side_length = 32;
alignas(16) g2dColor fade_rect[fade_rect_side_length * fade_rect_side_length];


void Platform::Screen::fade(Float amount,
                            ColorConstant k,
                            Optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{
    fade_include_sprites = include_sprites;
    fade_include_overlay = include_overlay;
    fade_amount = amount;
    fade_color = make_color((int)k, amount * 255);

    if (base) {
        fade_color = make_color((int)k, 255);
        const auto base_color = make_color((int)*base, 255);

        fade_color = G2D_RGBA(
            interpolate(G2D_GET_R(fade_color), G2D_GET_R(base_color), amount),
            interpolate(G2D_GET_G(fade_color), G2D_GET_G(base_color), amount),
            interpolate(G2D_GET_B(fade_color), G2D_GET_B(base_color), amount),
            255);
    }

    for (int i = 0; i < fade_rect_side_length; ++i) {
        for (int j = 0; j < fade_rect_side_length; ++j) {
            fade_rect[i + j * fade_rect_side_length] = fade_color;
        }
    }
}


void Platform::Screen::pixelate(u8 amount,
                                bool include_overlay,
                                bool include_background,
                                bool include_sprites)
{
}


static void
set_sprite_params(const Platform::Screen& screen, const Sprite& spr, int width)
{
    // const auto view_center = screen.get_view().get_center();

    // const auto pos = spr.get_position() - spr.get_origin().cast<Float>();

    // auto abs_position = pos - view_center;

    // if (spr.get_flip().x) {
    //     abs_position.x += width / 2;
    // }

    // if (spr.get_flip().y) {
    //     abs_position.y += 32;
    // }

    // if (auto rot = spr.get_rotation()) {
    //     g2dSetCoordMode(G2D_CENTER);
    //     abs_position.x += width / 4;
    //     abs_position.y += 16;
    //     g2dSetRotation(
    //         360 * ((float)rot / std::numeric_limits<Sprite::Rotation>::max()));
    // } else {
    //     g2dSetCoordMode(G2D_UP_LEFT);
    // }

    // abs_position.x *= 2.f;
    // abs_position.y *= 2.f;

    // // Unless you want lots of graphical artifacts, better to constrain your
    // // graphics to pixel boundaries.
    // auto integer_pos = abs_position.cast<s32>();

    // g2dSetCoordXY(integer_pos.x, integer_pos.y);
    // if (spr.get_size() not_eq Sprite::Size::w32_h32) {
    //     if (spr.get_texture_index() % 2) {
    //         g2dSetCropXY(32, 0);
    //         g2dSetCropWH(32, 64);
    //     } else {
    //         g2dSetCropXY(0, 0);
    //         g2dSetCropWH(32, 64);
    //     }
    // }
    // if (spr.get_alpha() == Sprite::Alpha::translucent) {
    //     // Glib2d has a bug where you have to set the alpha twice.
    //     g2dSetAlpha(128);
    //     g2dSetAlpha(128);
    // }
    // if (spr.get_flip().x or spr.get_flip().y) {
    //     Float x_scale = 1.f;
    //     Float y_scale = 1.f;

    //     if (spr.get_flip().x) {
    //         x_scale = -1;
    //     }

    //     if (spr.get_flip().y) {
    //         y_scale = -1;
    //     }

    //     g2dSetScale(x_scale, y_scale);

    // } else if (spr.get_scale().x or spr.get_scale().y) {

    //     g2dSetCoordMode(G2D_CENTER);
    //     integer_pos.x += width / 2;
    //     integer_pos.y += 32;
    //     g2dSetCoordXY(integer_pos.x, integer_pos.y);

    //     float x_scale = 1.f;
    //     float y_scale = 1.f;

    //     if (spr.get_scale().x < 0.f) {
    //         x_scale = 1.f * (1.f - float(spr.get_scale().x * -1) / (500.f));
    //     }

    //     if (spr.get_scale().y < 0.f) {
    //         y_scale = 1.f * (1.f - float(spr.get_scale().y * -1) / (500.f));
    //     }

    //     g2dSetScale(x_scale, y_scale);
    // }
}


static void display_sprite(const Platform::Screen& screen, const Sprite& spr)
{
    if (spr.get_alpha() == Sprite::Alpha::transparent) {
        return;
    }

    // g2dTexture temp;
    // temp.tw = 64;
    // temp.th = 64;
    // temp.h = 64;
    // temp.swizzled = false;

    // int width = 64;

    // if (spr.get_size() == Sprite::Size::w32_h32) {
    //     temp.data = sprite_image_ram[spr.get_texture_index()].pixels_;
    // } else {
    //     width = 32;
    //     temp.data = sprite_image_ram[spr.get_texture_index() / 2].pixels_;
    // }

    // temp.w = width;

    // if (not(spr.get_mix().color_ not_eq ColorConstant::null and
    //         spr.get_mix().amount_ == 255)) {

    //     g2dBeginRects(&temp);

    //     set_sprite_params(screen, spr, width);

    //     g2dAdd();
    //     g2dEnd();
    // }

    // if (spr.get_mix().color_ not_eq ColorConstant::null) {

    //     if (spr.get_size() == Sprite::Size::w32_h32) {
    //         temp.data = sprite_image_mask[spr.get_texture_index()].pixels_;
    //     } else {
    //         width = 32;
    //         temp.data = sprite_image_mask[spr.get_texture_index() / 2].pixels_;
    //     }

    //     g2dBeginRects(&temp);

    //     set_sprite_params(screen, spr, width);

    //     g2dSetColor(make_color((int)spr.get_mix().color_, 255));

    //     g2dSetAlpha(spr.get_mix().amount_);
    //     g2dSetAlpha(spr.get_mix().amount_);

    //     g2dAdd();
    //     g2dEnd();
    // }
}


static void display_map(const Vec2<Float>& view_offset)
{
    // g2dTexture temp;
    // temp.tw = 64;
    // temp.th = 64;
    // temp.w = 64;
    // temp.h = 48;
    // temp.swizzled = false;

    // const auto fixed_offset = view_offset.cast<s32>();

    // auto disp_layer = [&](u8 map_tiles[16][20],
    //                       TileMemory map_image_ram[map_image_ram_capacity]) {
    //     // Because many tiles will be repeated, we are going to batch the draws
    //     // for similar tiles.

    //     const int tiles_screen_capacity_heuristic = 48;
    //     Buffer<Vec2<int>, tiles_screen_capacity_heuristic>
    //         disp_buffers[map_image_ram_capacity];

    //     for (int x = 0; x < 16; ++x) {
    //         const auto x_target = x * 64 - (fixed_offset.x);
    //         if (x_target < -64 or x_target > 480) {
    //             continue;
    //         }

    //         for (int y = 0; y < 20; ++y) {
    //             const auto y_target = y * 48 - (fixed_offset.y);
    //             if (y_target < -48 or y_target > 272) {
    //                 continue;
    //             }

    //             const auto tile = map_tiles[x][y];
    //             if (tile not_eq 0) {
    //                 disp_buffers[tile].push_back(Vec2<int>{x_target, y_target});
    //             }
    //         }
    //     }

    //     for (int i = 0; i < map_image_ram_capacity; ++i) {
    //         auto& buffer = disp_buffers[i];
    //         temp.data = map_image_ram[i].pixels_;
    //         g2dBeginRects(&temp);
    //         g2dSetCoordMode(G2D_UP_LEFT);
    //         for (auto& pos : buffer) {
    //             g2dSetCoordXY(pos.x, pos.y);
    //             g2dAdd();
    //         }
    //         g2dEnd();
    //     }
    // };

    // disp_layer(map0_tiles, map0_image_ram);
    // disp_layer(map1_tiles, map1_image_ram);
}


static void display_overlay()
{
    // g2dTexture temp;
    // temp.tw = 16;
    // temp.th = 16;
    // temp.w = 16;
    // temp.h = 16;
    // temp.swizzled = false;

    // const auto origin = (overlay_origin * 2.f).cast<s32>();

    // for (int x = 0; x < 32; ++x) {
    //     for (int y = 0; y < 32; ++y) {
    //         auto tile = overlay_tiles[x][y];
    //         bool is_glyph = false;
    //         Optional<int> ext_palette;

    //         if (tile & (1 << 15)) {
    //             tile = tile & (~(1 << 15));
    //             is_glyph = true;

    //             if (tile & (1 << 10)) {
    //                 ext_palette = (tile & (0xf << 11)) >> 11;
    //                 tile = tile & (~(0xf << 11));
    //                 tile = tile & (~(1 << 10));
    //             }
    //         }

    //         if (tile) {
    //             if (is_glyph) {
    //                 temp.data = charset_image_ram[tile].pixels_;
    //             } else {
    //                 temp.data = overlay_image_ram[tile].pixels_;
    //             }
    //             g2dBeginRects(&temp);
    //             g2dSetCoordMode(G2D_UP_LEFT);
    //             g2dSetCoordXY(x * 16 - origin.x, y * 16 - origin.y);
    //             if (is_glyph) {
    //                 if (ext_palette) {
    //                     auto& colors = font_extra_palettes[*ext_palette];
    //                     g2dSetColor(make_color((int)colors.background_, 255));
    //                 } else {
    //                     g2dSetColor(default_text_background_color);
    //                 }
    //             }
    //             g2dAdd();
    //             g2dEnd();

    //             if (is_glyph) {
    //                 temp.data = charset_image_ram2[tile].pixels_;
    //                 g2dBeginRects(&temp);
    //                 g2dSetCoordMode(G2D_UP_LEFT);
    //                 g2dSetCoordXY(x * 16 - origin.x, y * 16 - origin.y);
    //                 if (ext_palette) {
    //                     auto& colors = font_extra_palettes[*ext_palette];
    //                     g2dSetColor(make_color((int)colors.foreground_, 255));
    //                 } else {
    //                     g2dSetColor(default_text_foreground_color);
    //                 }
    //                 g2dAdd();
    //                 g2dEnd();
    //             }
    //         }
    //     }
    // }
}


static void display_background(const Vec2<Float>& view_offset)
{
    // g2dTexture temp;
    // temp.tw = 64;
    // temp.th = 64;
    // temp.w = 16;
    // temp.h = 16;
    // temp.swizzled = false;

    // Buffer<Vec2<int>, 32 * 32> star_buffer_1;
    // Buffer<Vec2<int>, 32 * 32> star_buffer_2;

    // const auto fixed_offset = view_offset.cast<s32>();

    // for (int x = 0; x < 32; ++x) {

    //     auto x_target = x * 16 - fixed_offset.x;

    //     if (x_target > 480) {
    //         x_target -= 32 * 16;
    //         // we try to wrap the tile, and if it's still out of bounds, don't
    //         // draw it.
    //         if (x_target > 480) {
    //             continue;
    //         }
    //     }

    //     if (x_target < -16) {
    //         x_target += 32 * 16;
    //         // we try to wrap the tile, and if it's still out of bounds, don't
    //         // draw it.
    //         if (x_target < -16 or x_target > 480) {
    //             continue;
    //         }
    //     }

    //     for (int y = 0; y < 32; ++y) {

    //         auto y_target = y * 16 - fixed_offset.y;

    //         if (y_target > 272) {
    //             y_target -= 32 * 16;
    //             // we try to wrap the tile, and if it's still out of bounds, don't
    //             // draw it.
    //             if (y_target > 272) {
    //                 continue;
    //             }
    //         }

    //         if (y_target < -16) {
    //             y_target += 32 * 16;
    //             // we try to wrap the tile, and if it's still out of bounds, don't
    //             // draw it.
    //             if (y_target < -16 or x_target > 272) {
    //                 continue;
    //             }
    //         }

    //         const auto tile = background_tiles[x][y];

    //         if (tile == 70) {
    //             star_buffer_1.push_back(Vec2<int>{x_target, y_target});
    //         } else if (tile == 71) {
    //             star_buffer_2.push_back(Vec2<int>{x_target, y_target});
    //         } else if (tile not_eq 60) {
    //             const auto tile_block = tile / 12;
    //             const auto block_offset = tile % 12;
    //             const int sub_x = block_offset % 4;
    //             const int sub_y = block_offset / 4;
    //             temp.data = map0_image_ram[tile_block].pixels_;
    //             g2dBeginRects(&temp);
    //             g2dSetCoordMode(G2D_UP_LEFT);
    //             g2dSetCoordXY(x_target, y_target);
    //             g2dSetCropXY(sub_x * 16, sub_y * 16);
    //             g2dSetCropWH(16, 16);
    //             g2dAdd();
    //             g2dEnd();
    //         }
    //     }
    // }

    // if (not star_buffer_1.empty()) {
    //     const auto tile_block = 70 / 12;
    //     const auto block_offset = 70 % 12;
    //     const int sub_x = block_offset % 4;
    //     const int sub_y = block_offset / 4;

    //     temp.data = map0_image_ram[tile_block].pixels_;

    //     g2dBeginRects(&temp);
    //     g2dSetCoordMode(G2D_UP_LEFT);

    //     g2dSetCropXY(sub_x * 16, sub_y * 16);
    //     g2dSetCropWH(16, 16);

    //     for (auto& pos : star_buffer_1) {
    //         g2dSetCoordXY(pos.x, pos.y);
    //         g2dAdd();
    //     }

    //     g2dEnd();
    // }

    // if (not star_buffer_2.empty()) {
    //     const auto tile_block = 71 / 12;
    //     const auto block_offset = 71 % 12;
    //     const int sub_x = block_offset % 4;
    //     const int sub_y = block_offset / 4;

    //     temp.data = map0_image_ram[tile_block].pixels_;

    //     g2dBeginRects(&temp);
    //     g2dSetCoordMode(G2D_UP_LEFT);

    //     g2dSetCropXY(sub_x * 16, sub_y * 16);
    //     g2dSetCropWH(16, 16);

    //     for (auto& pos : star_buffer_2) {
    //         g2dSetCoordXY(pos.x, pos.y);
    //         g2dAdd();
    //     }

    //     g2dEnd();
    // }
}


static void display_fade()
{
    if (fade_amount == 0.f) {
        return;
    }

    g2dTexture temp;
    temp.tw = fade_rect_side_length;
    temp.th = fade_rect_side_length;
    temp.h = fade_rect_side_length;
    temp.w = fade_rect_side_length;
    temp.swizzled = false;
    temp.data = fade_rect;

    g2dBeginRects(&temp);
    g2dSetCoordMode(G2D_UP_LEFT);

    for (int x = 0; x < (480 / fade_rect_side_length); ++x) {
        for (int y = 0;
             y < (272 / fade_rect_side_length) + fade_rect_side_length;
             ++y) {
            g2dSetCoordXY(x * 32, y * 32);
            g2dAdd();
        }
    }

    g2dEnd();
}


void Platform::Screen::display()
{
    display_background((view_.get_center() * 0.3f) * 2.f);

    display_map(view_.get_center() * 2.f);

    for (auto& spr : reversed(::sprite_queue)) {
        display_sprite(*this, spr);
    }

    sprite_queue.clear();

    display_fade();

    display_overlay();

    g2dFlip(G2D_VSYNC);
}


////////////////////////////////////////////////////////////////////////////////
// Speaker
////////////////////////////////////////////////////////////////////////////////


Platform::Speaker::Speaker()
{
}


void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   Optional<Vec2<Float>> position)
{
    // TODO
}


bool Platform::Speaker::is_sound_playing(const char* name)
{
    // TODO
    return false;
}


Microseconds Platform::Speaker::track_length(const char* sound_or_music_name)
{
    // TODO
    return 0;
}


void Platform::Speaker::play_music(const char* name, Microseconds offset)
{
    // TODO
}


void Platform::Speaker::stop_music()
{
    // TODO
}


bool Platform::Speaker::is_music_playing(const char* name)
{
    // TODO
    return false;
}


////////////////////////////////////////////////////////////////////////////////
// Logger
////////////////////////////////////////////////////////////////////////////////


static Severity log_threshold;


void Platform::Logger::set_threshold(Severity severity)
{
    log_threshold = severity;
}


void Platform::Logger::log(Severity severity, const char* msg)
{
    int fd;
    if (!(fd = sceIoOpen("ms0:/blindjump_logfile.txt",
                         PSP_O_WRONLY | PSP_O_CREAT | PSP_O_APPEND,
                         0777))) {
        return;
    }

    const char* snames[(int)Severity::count] = {
        "[DEBUG] ",
        "[INFO] ",
        "[WARNING] ",
        "[ERROR] ",
        "[FATAL] ",
    };

    sceIoWrite(fd, snames[(int)severity], strlen(snames[(int)severity]));
    sceIoWrite(fd, msg, strlen(msg));
    sceIoWrite(fd, "\n", 1);

    sceIoClose(fd);
}


Platform::Logger::Logger()
{
}


////////////////////////////////////////////////////////////////////////////////
// RemoteConsole
////////////////////////////////////////////////////////////////////////////////


auto Platform::RemoteConsole::readline() -> Optional<Line>
{
    // TODO
    return {};
}


////////////////////////////////////////////////////////////////////////////////
// Keyboard
////////////////////////////////////////////////////////////////////////////////


Optional<Bitvector<int(Key::count)>> missed_keys;


void Platform::Keyboard::rumble(bool)
{
    // PSP hardware does not support rumble/vibrate.
}


void Platform::Keyboard::poll()
{
    std::copy(std::begin(states_), std::end(states_), std::begin(prev_));

    SceCtrlData buttonInput;

    sceCtrlPeekBufferPositive(&buttonInput, 1);

    states_[int(Key::start)] = buttonInput.Buttons & PSP_CTRL_START;
    states_[int(Key::select)] = buttonInput.Buttons & PSP_CTRL_SELECT;
    states_[int(Key::right)] = buttonInput.Buttons & PSP_CTRL_RIGHT;
    states_[int(Key::left)] = buttonInput.Buttons & PSP_CTRL_LEFT;
    states_[int(Key::down)] = buttonInput.Buttons & PSP_CTRL_DOWN;
    states_[int(Key::up)] = buttonInput.Buttons & PSP_CTRL_UP;
    states_[int(Key::alt_1)] = buttonInput.Buttons & PSP_CTRL_LTRIGGER;
    states_[int(Key::alt_2)] = buttonInput.Buttons & PSP_CTRL_RTRIGGER;
    states_[int(Key::action_1)] = buttonInput.Buttons & PSP_CTRL_CROSS;
    states_[int(Key::action_2)] = buttonInput.Buttons & PSP_CTRL_CIRCLE;

    const u8 stick_x = buttonInput.Lx;
    const u8 stick_y = buttonInput.Ly;

    static const int dead_zone = 50;
    if (stick_x < 128 - dead_zone) {
        states_[int(Key::left)] = true;
    } else if (stick_x > 128 + dead_zone) {
        states_[int(Key::right)] = true;
    }

    if (stick_y < 128 - dead_zone) {
        states_[int(Key::up)] = true;
    } else if (stick_y > 128 + dead_zone) {
        states_[int(Key::down)] = true;
    }

    if (UNLIKELY(static_cast<bool>(::missed_keys))) {
        for (int i = 0; i < (int)Key::count; ++i) {
            if ((*::missed_keys)[i]) {
                states_[i] = true;
            }
        }
        ::missed_keys.reset();
    }
}


void Platform::sleep(Frame frames)
{
    u64 sleep_start = 0;
    sceRtcGetCurrentTick(&sleep_start);

    Keyboard temp_kb;

    const auto start_keys = keyboard_.dump_state();

    while (frames) {
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

        --frames;
        sceDisplayWaitVblankStart();
    }

    u64 sleep_end = 0;
    sceRtcGetCurrentTick(&sleep_end);

    ::slept_ticks += sleep_end - sleep_start;
}
