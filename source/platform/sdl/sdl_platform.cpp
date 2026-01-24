////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "number/random.hpp"
#include "platform/conf.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <queue>
#include <sstream>
#include <thread>
#include <unordered_set>
#if defined(__APPLE__)
#include <mach-o/dyld.h> // for _NSGetExecutablePath
#include <unistd.h>      // for fork, execl
#elif defined(__linux__)
#include <unistd.h> // for fork, execl, readlink
#elif defined(_WIN32)
#include <windows.h> // for GetModuleFileNameA, CreateProcessA
#endif



#ifdef _WIN32
#define PATH_DELIMITER "\\"
#else
#define PATH_DELIMITER "/"
#endif



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



bool console_running = false;



static void set_gflag(GlobalFlag f, bool val)
{
    gflags.set(static_cast<int>(f), val);
}


static bool get_gflag(GlobalFlag f)
{
    return gflags.get(static_cast<int>(f));
}



std::string resource_path();



void start(Platform&);



SDL_Window* window = nullptr;
static SDL_Renderer* renderer = nullptr;
bool sdl_running = true;
static bool is_fullscreen = false;
static int window_scale = 2;
static const int logical_width = 240;
static const int logical_height = 160;

static bool window_size_initialized = false;
static int last_window_width = 0;
static int last_window_height = 0;

static SDL_Texture* tile_recolor_buffer = nullptr;


static int circle_effect_radius = 0;
static int circle_effect_origin_x = 0;
static int circle_effect_origin_y = 0;

struct PngPalette
{
    SDL_Color colors[256];
    int count = 0;
    bool found = false;
};

// Store shader-transformed palettes for tile encoding
static PngPalette overlay_transformed_palette;
static PngPalette tile0_transformed_palette;
static PngPalette tile1_transformed_palette;
static PngPalette sprite_transformed_palette;
static PngPalette background_transformed_palette;


static SDL_Texture* background_texture = nullptr;
static SDL_Surface* background_surface = nullptr;
static int background_texture_width = 0;
static int background_texture_height = 0;

static SDL_Texture* current_overlay_texture = nullptr;
static int overlay_texture_width = 0;
static int overlay_texture_height = 0;

// For dynamic glyph mapping, we'll extend the texture
static SDL_Surface* overlay_surface = nullptr;
static int overlay_base_tile_count =
    0; // How many tiles in the original texture

static SDL_Texture* tile0_texture = nullptr;
static SDL_Surface* tile0_surface = nullptr;
static int tile0_texture_width = 0;
static int tile0_texture_height = 0;

static SDL_Texture* tile1_texture = nullptr;
static SDL_Surface* tile1_surface = nullptr;
static int tile1_texture_width = 0;
static int tile1_texture_height = 0;


static std::map<std::string, SDL_Surface*> charset_surfaces;
static std::map<std::string, SDL_Surface*> overlay_source_cache;


static int process_argc;
static char** process_argv;


static bool parallax_clouds_enabled = false;
static bool vertical_parallax_enabled = false;


struct ParallaxStrip
{
    int start_tile_row;
    int end_tile_row; // inclusive
    Vec2<s32> scroll;
};

static ParallaxStrip parallax_strip1 = {0, 13, {0, 0}};  // gradient
static ParallaxStrip parallax_strip2 = {14, 15, {0, 0}}; // dark clouds
static ParallaxStrip parallax_strip3 = {16, 19, {0, 0}}; // white clouds

static bool tile0_translucent = false;
static bool tile1_translucent = false;


struct PointLight
{
    SDL_Texture* texture;
    int radius;
};


static std::map<int, PointLight> point_light_cache;
static std::vector<std::tuple<Fixnum, Fixnum, int, ColorConstant, u8>>
    point_lights;


static ColorConstant sdl_color_to_colorconstant(const SDL_Color& c)
{
    // Quantize to 5-bit color like GBA hardware does
    u8 r = (c.r >> 3) << 3;
    u8 g = (c.g >> 3) << 3;
    u8 b = (c.b >> 3) << 3;

    return (ColorConstant)((r << 16) | (g << 8) | b);
}


SDL_Color color_to_sdl(ColorConstant k)
{
    u32 hex = (u32)k;

    // Extract RGB from hex color
    u8 r = (hex >> 16) & 0xFF;
    u8 g = (hex >> 8) & 0xFF;
    u8 b = hex & 0xFF;

    r = (r >> 3) << 3;
    g = (g >> 3) << 3;
    b = (b >> 3) << 3;

    SDL_Color result = {r, g, b, 255};
    return result;
}



static std::map<SDL_Scancode, Key> keymap;


bool save_surface_to_bmp(SDL_Surface* surface, const char* filename)
{
    if (SDL_SaveBMP(surface, filename) != 0) {
        std::cerr << "Failed to save BMP: " << SDL_GetError() << std::endl;
        return false;
    }

    std::cout << "Saved surface to " << filename << " (" << surface->w << "x"
              << surface->h << ")" << std::endl;
    return true;
}



static SDL_Texture* create_point_light_texture(int radius)
{
    if (not renderer) {
        return nullptr;
    }
    if (radius <= 0) {
        return nullptr;
    }

    // Check cache first
    auto it = point_light_cache.find(radius);
    if (it != point_light_cache.end()) {
        return it->second.texture;
    }

    // Create a new light texture
    int size = radius * 2;
    SDL_Surface* surface = SDL_CreateRGBSurface(
        0, size, size, 32, 0x000000FF, 0x0000FF00, 0x00FF0000, 0xFF000000);

    if (!surface) {
        error(
            format("Failed to create point light surface: %", SDL_GetError()));
        return nullptr;
    }

    // Lock surface for pixel access
    if (SDL_MUSTLOCK(surface)) {
        if (SDL_LockSurface(surface) != 0) {
            error(format("Failed to lock surface: %", SDL_GetError()));
            SDL_FreeSurface(surface);
            return nullptr;
        }
    }

    Uint32* pixels = (Uint32*)surface->pixels;
    float center = radius;

    const int falloff_type = 2;

    // Generate radial gradient
    for (int y = 0; y < size; y++) {
        for (int x = 0; x < size; x++) {
            float dx = x - center;
            float dy = y - center;
            float distance = sqrt(dx * dx + dy * dy);

            float t = 1.0f - (distance / radius);
            t = std::max(0.0f, t);

            float intensity;
            switch (falloff_type) {
            case 0: // Quadratic
                intensity = t * t;
                break;
            case 1: // Smoothstep
                intensity = t * t * (3.0f - 2.0f * t);
                break;
            case 2: // Cubic
                intensity = t * t * t;
                break;
            case 3: // Exponential
                intensity = exp(-4.0f * (1.0f - t)) / exp(-4.0f);
                break;
            case 4: // Inverse square (physically accurate)
                intensity =
                    1.0f / (1.0f + (distance * distance) / (radius * radius));
                break;
            default:
                intensity = t * t;
            }

            u8 value = (u8)(intensity * 255);
            Uint32 pixel =
                SDL_MapRGBA(surface->format, value, value, value, 255);
            pixels[y * size + x] = pixel;
        }
    }

    if (SDL_MUSTLOCK(surface)) {
        SDL_UnlockSurface(surface);
    }

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    SDL_FreeSurface(surface);

    if (!texture) {
        error(
            format("Failed to create point light texture: %", SDL_GetError()));
        return nullptr;
    }

    SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_ADD);

    point_light_cache[radius] = {texture, radius};

    return texture;
}


static void cleanup_point_light_cache()
{
    for (auto& [radius, light] : point_light_cache) {
        if (light.texture) {
            SDL_DestroyTexture(light.texture);
        }
    }
    point_light_cache.clear();
}


struct RectInfo
{
    int x, y, w, h;
    ColorConstant tint;
    int priority;
};


std::vector<RectInfo> rect_draw_queue;


static const Platform::Extensions extensions{
    .overlay_circle_effect =
        [](int radius, int x, int y) {
            circle_effect_radius = radius;
            circle_effect_origin_x = x;
            circle_effect_origin_y = y;
        },
    .quit = []() {
        sdl_running = false;
    },
    .vertical_parallax_enable = [](bool on) { vertical_parallax_enabled = on; },
    .enable_parallax_clouds = [](bool on) { parallax_clouds_enabled = on; },
    .sprite_overlapping_supported = [](bool& result) { result = true; },
    .has_startup_opt = [](const char* opt) -> bool {
        for (int i = 0; i < process_argc; ++i) {
            if (str_eq(process_argv[i], opt)) {
                return true;
            }
        }
        return false;
    },
    .update_parallax_r1 =
        [](u8 scroll) {
            auto& screen = PLATFORM.screen();
            auto center = screen.get_view().int_center().cast<s32>();
            if (vertical_parallax_enabled) {
                parallax_strip2.scroll.x = scroll + (center.x / 3);
                parallax_strip2.scroll.y = center.y / 4 + 3;
                parallax_strip1.scroll.y = center.y / 4 + 3;
            } else {
                parallax_strip2.scroll.x = scroll + (center.x / 3);
                parallax_strip2.scroll.y = center.y / 2;
                parallax_strip1.scroll.y = 0;
            }
        },
    .update_parallax_r2 =
        [](u8 scroll) {
            auto& screen = PLATFORM.screen();
            auto center = screen.get_view().int_center().cast<s32>();
            if (vertical_parallax_enabled) {
                parallax_strip3.scroll.x = scroll + (center.x / 3);
                parallax_strip3.scroll.y = center.y / 2 * 0.7f + 3;
            } else {
                parallax_strip3.scroll.x = scroll + (center.x / 3);
                parallax_strip3.scroll.y = center.y / 2;
            }
        },
    .draw_point_light =
        [](Fixnum x, Fixnum y, int radius, ColorConstant tint, u8 intensity) {
            if (radius <= 0 or intensity == 0) {
                return;
            }

            point_lights.push_back({x, y, radius, tint, intensity});
        },
    .enable_translucence =
        [](const Buffer<Layer, 4>& layers) {
            // Reset translucence flags
            tile0_translucent = false;
            tile1_translucent = false;

            // Set translucence for requested layers
            for (auto& layer : layers) {
                if (layer == Layer::map_0_ext || layer == Layer::map_0) {
                    tile0_translucent = true;
                }
                if (layer == Layer::map_1_ext || layer == Layer::map_1) {
                    tile1_translucent = true;
                }
            }
        },
    .draw_rect =
        [](int x, int y, int w, int h, ColorConstant tint, int priority) {
            rect_draw_queue.push_back({x, y, w, h, tint, priority});
        },
    .map_key =
        [](Key k, const char* key_name) {
            if (strlen(key_name) == 0) {
                return;
            }
            SDL_Scancode scancode = SDL_GetScancodeFromName(key_name);

            if (scancode == SDL_SCANCODE_UNKNOWN) {
                warning(format("unknown scancode %", key_name));
                return;
            }

            // info(format("mapped % to %", (int)k, (int)scancode));

            // We need to erase the existing mapping for this key
            for (auto it = keymap.begin(); it not_eq keymap.end();) {
                if (it->second == k) {
                    it = keymap.erase(it);
                } else {
                    ++it;
                }
            }
            keymap[scancode] = k;
        }};



const Platform::Extensions& Platform::get_extensions()
{
    return extensions;
}



static int calculate_best_scale(int window_w, int window_h)
{
    // Calculate maximum scale that fits in both dimensions
    int scale_x = window_w / logical_width;
    int scale_y = window_h / logical_height;

    // Use the smaller scale to ensure it fits
    int scale = std::min(scale_x, scale_y);

    // Clamp to minimum of 1
    return std::max(1, scale);
}



static void constrain_window_to_scale()
{
    if (is_fullscreen) {
        return; // Don't constrain in fullscreen
    }

    int current_w, current_h;
    SDL_GetWindowSize(window, &current_w, &current_h);

    // Calculate the best integer scale for current size
    int scale = calculate_best_scale(current_w, current_h);

    // Calculate the exact size for this scale
    int target_w = logical_width * scale;
    int target_h = logical_height * scale;

    // Check if we're closer to the next scale up
    int next_scale = scale + 1;
    int next_w = logical_width * next_scale;
    int next_h = logical_height * next_scale;

    // Calculate distance to current scale and next scale
    int dist_current = abs(current_w - target_w) + abs(current_h - target_h);
    int dist_next = abs(current_w - next_w) + abs(current_h - next_h);

    // Use whichever is closer
    if (dist_next < dist_current) {
        scale = next_scale;
        target_w = next_w;
        target_h = next_h;
    }

    // Only resize if different from current size
    if (current_w != target_w || current_h != target_h) {
        SDL_SetWindowSize(window, target_w, target_h);
        window_scale = scale;

        info(format(
            "Window resized to %x scale (%x%)", scale, target_w, target_h));
    }
}



static void update_viewport()
{
    if (not window) {
        return;
    }
    if (not renderer) {
        return;
    }

    int window_w, window_h;
    SDL_GetWindowSize(window, &window_w, &window_h);

    SDL_Rect viewport;

    if (is_fullscreen) {
        // In fullscreen, use integer scaling with letterboxing
        int scale = calculate_best_scale(window_w, window_h);
        int scaled_width = logical_width * scale;
        int scaled_height = logical_height * scale;

        // Center the viewport
        viewport.x = (window_w - scaled_width) / 2;
        viewport.y = (window_h - scaled_height) / 2;
        viewport.w = scaled_width;
        viewport.h = scaled_height;
    } else {
        // In windowed mode, use exact window size (already constrained)
        viewport.x = 0;
        viewport.y = 0;
        viewport.w = window_w;
        viewport.h = window_h;
    }

    SDL_RenderSetViewport(renderer, &viewport);
    SDL_RenderSetLogicalSize(renderer, logical_width, logical_height);

    // Use nearest neighbor (point) filtering for crisp pixels
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "nearest");
}



static void toggle_fullscreen()
{
    if (not window) {
        return;
    }

    is_fullscreen = !is_fullscreen;

    if (is_fullscreen) {
        // Store current windowed size
        SDL_GetWindowSize(window, &last_window_width, &last_window_height);

        // Get desktop display mode for current display
        SDL_DisplayMode display_mode;
        int display_index = SDL_GetWindowDisplayIndex(window);
        SDL_GetDesktopDisplayMode(display_index, &display_mode);

        // On macOS, use native fullscreen for better behavior
#ifdef __APPLE__
        SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
#else
        SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
#endif

        info(format(
            "Entered fullscreen mode: %x%", display_mode.w, display_mode.h));
    } else {
        SDL_SetWindowFullscreen(window, 0);

        if (last_window_width > 0 && last_window_height > 0) {
            SDL_SetWindowSize(window, last_window_width, last_window_height);
        } else {
            // Default to current scale
            SDL_SetWindowSize(window,
                              logical_width * window_scale,
                              logical_height * window_scale);
        }

        info("Exited fullscreen mode");
    }

    update_viewport();
}



static void handle_window_resize(int w, int h)
{
    // Ignore resize events in fullscreen (macOS sends these during transitions)
    if (is_fullscreen) {
        update_viewport();
        return;
    }

    // Constrain to integer scale
    constrain_window_to_scale();
    update_viewport();
}



static void cycle_window_scale(bool increase)
{
    if (not window) {
        return;
    }
    if (is_fullscreen) {
        return; // Don't change scale in fullscreen
    }

    if (increase) {
        window_scale = std::min(window_scale + 1, 8); // Max 8x
    } else {
        window_scale = std::max(window_scale - 1, 1); // Min 1x
    }

    int new_w = logical_width * window_scale;
    int new_h = logical_height * window_scale;
    SDL_SetWindowSize(window, new_w, new_h);

    update_viewport();
}



Platform* __platform__ = nullptr;



int main(int argc, char** argv)
{
    process_argc = argc;
    process_argv = argv;

    for (int i = 0; i < argc; ++i) {
        if (str_eq(argv[i], "--help")) {
            std::cout << "usage: skyland [optional-flags] \n"
                      << " --regression        Run test cases, regression "
                "tests, and then exit \n"
                      << " --validate-scripts  Just run syntax checks on "
                "scripts, and then exit\n"
                      << " --no-window-system  Run a windowless instance of the game\n"
                      << std::endl;
            return EXIT_SUCCESS;
        }
    }

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    SDL_SetHint(SDL_HINT_VIDEO_HIGHDPI_DISABLED, "0");

    int initial_width = logical_width * window_scale;
    int initial_height = logical_height * window_scale;

    if (not extensions.has_startup_opt("--regression") and
        not extensions.has_startup_opt("--no-window-system")) {
        window = SDL_CreateWindow("Skyland",
                                  SDL_WINDOWPOS_UNDEFINED,
                                  SDL_WINDOWPOS_UNDEFINED,
                                  initial_width,
                                  initial_height,
                                  SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE |
                                  SDL_WINDOW_ALLOW_HIGHDPI);

        if (window == NULL) {
            fprintf(
                    stderr, "SDL window failed to initialise: %s\n", SDL_GetError());
            return 1;
        }

        // Set minimum window size to 1x scale
        SDL_SetWindowMinimumSize(window, logical_width, logical_height);

        renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);

        if (renderer == NULL) {
            fprintf(
                    stderr, "SDL renderer failed to initialise: %s\n", SDL_GetError());
            return 1;
        }

        // Enable integer scaling for crisp pixels
        SDL_RenderSetIntegerScale(renderer, SDL_TRUE);

        tile_recolor_buffer = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, 16, 16);
    }

    rng::critical_state = time(nullptr);

    Platform pf;

    __platform__ = &pf;
    start(pf);

    if (renderer) {
    SDL_DestroyRenderer(renderer);
    }
    if (window) {
    SDL_DestroyWindow(window);
    }
    SDL_Quit();
}



////////////////////////////////////////////////////////////////////////////////
// Misc Platform stuff...
////////////////////////////////////////////////////////////////////////////////


void Platform::restart()
{
#if defined(__APPLE__) || defined(__linux__)
    // Get the executable path
    char path[1024];
#ifdef __APPLE__
    uint32_t size = sizeof(path);
    _NSGetExecutablePath(path, &size);
#else // Linux
    ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
    if (len != -1)
        path[len] = '\0';
#endif

    // Fork and exec
    if (fork() == 0) {
        execl(path, path, NULL);
        exit(1); // If exec fails
    }
    exit(0);

#elif defined(_WIN32)
    char path[MAX_PATH];
    GetModuleFileNameA(NULL, path, MAX_PATH);

    STARTUPINFOA si = {sizeof(si)};
    PROCESS_INFORMATION pi;

    if (CreateProcessA(
            path, NULL, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
    }
    exit(0);
#else
    exit(EXIT_SUCCESS);
#endif
}



struct TileInfo
{
    TileDesc tile_desc;
    u16 palette;
    ColorConstant text_fg_color_;
    ColorConstant text_bg_color_;
};



static std::map<Layer, std::map<std::pair<u16, u16>, TileInfo>> tile_layers_;



static const std::map<u16, SDL_Color> special_palettes = {
    {15, {0xef, 0x0d, 0x54, 255}}, // Red (custom_color(0xef0d54))
    {14, {0x10, 0x31, 0x63, 255}}, // Blue-black (custom_color(0x103163))
    {13, {0xff, 0xff, 0xff, 255}}, // White (silver_white)
};



TileDesc Platform::get_tile(Layer layer, u16 x, u16 y)
{
    if (layer == Layer::overlay) {
        x %= 32;
        y %= 32;
    }

    auto it = tile_layers_[layer].find({x, y});
    if (it != tile_layers_[layer].end()) {
        return it->second.tile_desc;
    }
    return 0;
}


static SDL_Color default_text_fg_color = {0xcd, 0xc3, 0xeb, 255};
static SDL_Color default_text_bg_color = {0x00, 0x00, 0x00, 255};



ColorConstant default_fg_color()
{
    u32 hex = (default_text_fg_color.r << 16) | (default_text_fg_color.g << 8) |
              default_text_fg_color.b;
    return (ColorConstant)hex;
}



bool is_glyph(TileDesc);



void Platform::clear_layer(Layer layer)
{
    if (layer == Layer::map_0_ext) {
        clear_layer(Layer::map_0);
    } else if (layer == Layer::map_1_ext) {
        clear_layer(Layer::map_1);
    }

    if (auto found = tile_layers_.find(layer);
        found not_eq tile_layers_.end()) {
        tile_layers_.erase(found);
    }
}



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        TileDesc val,
                        Optional<u16> palette)
{
    auto erase_existing = [&](auto& layer, u16 x, u16 y) {
        auto found_tile = layer.find({x, y});
        if (found_tile not_eq layer.end()) {
            layer.erase(found_tile);
        }
    };

    switch (layer) {
    case Layer::map_0_ext: {
        auto found_layer = tile_layers_.find(Layer::map_0);
        if (found_layer not_eq tile_layers_.end()) {
            // On GBA hardware, map_0_ext is just a special addressing mode that
            // treats tiles in the map_0 layer as 16x16 blocks of 2x2
            // metatiles. Therefore, when we write to map_0_ext, we erase the
            // overlapping contents of map_0, because the game assumes that
            // they're effectively the same tile layer.
            erase_existing(found_layer->second, x * 2, y * 2);
            erase_existing(found_layer->second, x * 2 + 1, y * 2);
            erase_existing(found_layer->second, x * 2, y * 2 + 1);
            erase_existing(found_layer->second, x * 2 + 1, y * 2 + 1);
        }
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0)};
        break;
    }

    case Layer::map_1_ext: {
        auto found_layer = tile_layers_.find(Layer::map_1);
        if (found_layer not_eq tile_layers_.end()) {
            erase_existing(found_layer->second, x * 2, y * 2);
            erase_existing(found_layer->second, x * 2 + 1, y * 2);
            erase_existing(found_layer->second, x * 2, y * 2 + 1);
            erase_existing(found_layer->second, x * 2 + 1, y * 2 + 1);
        }
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0)};
        break;
    }

    case Layer::map_0:
    case Layer::map_1:
    case Layer::background:
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0)};
        break;

    case Layer::overlay:
        x %= 32;
        y %= 32;
        ColorConstant fg_color = custom_color(0);
        if (is_glyph(val)) {
            fg_color = default_fg_color();
        }
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0), fg_color};
        break;
    }
}



void Platform::Keyboard::rumble(bool enabled)
{
}



const char* Platform::Keyboard::check_key()
{
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        switch (e.type) {
        case SDL_KEYDOWN:
            return SDL_GetScancodeName(e.key.keysym.scancode);
            break;

        case SDL_QUIT:
            exit(1);
            break;

        default:
            break;
        }
    }
    return nullptr;
}



void Platform::Keyboard::poll()
{
    std::copy(std::begin(states_), std::end(states_), std::begin(prev_));

    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        switch (e.type) {
        case SDL_QUIT:
            sdl_running = false;
            break;

        case SDL_WINDOWEVENT:
            switch (e.window.event) {
            case SDL_WINDOWEVENT_RESIZED:
            case SDL_WINDOWEVENT_SIZE_CHANGED:
                handle_window_resize(e.window.data1, e.window.data2);
                break;
            case SDL_WINDOWEVENT_FOCUS_GAINED:
                // On macOS, re-constrain when window gains focus
                // This helps with issues after fullscreen transitions
                if (!window_size_initialized) {
                    constrain_window_to_scale();
                    window_size_initialized = true;
                }
                break;
            }
            break;

        case SDL_KEYDOWN: {
            // F11 or Cmd+F (macOS) or Alt+Enter for fullscreen
            if (e.key.keysym.scancode == SDL_SCANCODE_F11 ||
                (e.key.keysym.scancode == SDL_SCANCODE_F &&
                 (e.key.keysym.mod & KMOD_GUI)) ||
                (e.key.keysym.scancode == SDL_SCANCODE_RETURN &&
                 (e.key.keysym.mod & KMOD_ALT))) {
                toggle_fullscreen();
                break; // Don't fall through
            }
            // Cmd/Ctrl + Plus/Minus to change window scale
            if ((e.key.keysym.mod & (KMOD_GUI | KMOD_CTRL))) {
                if (e.key.keysym.scancode == SDL_SCANCODE_EQUALS ||
                    e.key.keysym.scancode == SDL_SCANCODE_KP_PLUS) {
                    cycle_window_scale(true);
                    break;
                } else if (e.key.keysym.scancode == SDL_SCANCODE_MINUS ||
                           e.key.keysym.scancode == SDL_SCANCODE_KP_MINUS) {
                    cycle_window_scale(false);
                    break;
                }
            }
            // Note: removed duplicate fullscreen check
            auto it = keymap.find(e.key.keysym.scancode);
            if (it != keymap.end()) {
                states_[int(it->second)] = true;
            }
            break;
        }

        case SDL_KEYUP: {
            auto it = keymap.find(e.key.keysym.scancode);
            if (it != keymap.end()) {
                states_[int(it->second)] = false;
            }
            break;
        }
        }
    }
}


static std::map<std::string, std::string> files;



std::pair<const char*, u32> Platform::load_file(const char* folder,
                                                const char* filename) const
{
    std::string name;
    if (strlen(folder)) {
        name += std::string(folder) + PATH_DELIMITER;
    }
    name += filename;

    std::string path;
    for (char c : name) {
        if (c == '/') {
            path += PATH_DELIMITER;
        } else {
            path += c;
        }
    }

    auto found = files.find(path);
    if (found == files.end()) {
        std::fstream file(resource_path() + path);
        if (not file) {
            warning(format("missing file %", (resource_path() + path).c_str()));
        }
        std::stringstream buffer;
        buffer << file.rdbuf();
        files[path] = buffer.str();
    } else {
        return {found->second.c_str(), found->second.length()};
    }

    found = files.find(path);
    return {found->second.c_str(), found->second.length()};
}



struct DynamicTextureMapping
{
    bool reserved_ = false;
    u16 spritesheet_offset_ = 0;
} dynamic_texture_mappings[Platform::dynamic_texture_count];



static inline bool is_dynamic_texture_index(u16 texture_index,
                                            Sprite::Size size)
{
    int scale = 2;
    if (size == Sprite::Size::w32_h32) {
        scale = 1;
    } else if (size == Sprite::Size::w16_h16) {
        scale = 4;
    }
    return texture_index < (Platform::dynamic_texture_count * scale);
}



static inline u8 get_dynamic_slot_for_index(u16 texture_index,
                                            Sprite::Size size)
{
    if (size == Sprite::Size::w32_h32) {
        return texture_index;
    } else if (size == Sprite::Size::w16_h16) {
        return texture_index / 4;
    }
    // Convert texture index to slot number
    // Indices 0-1 -> slot 0, indices 2-3 -> slot 1, etc.
    return texture_index / 2;
}



void Platform::DynamicTexture::remap(u16 spritesheet_offset)
{
    auto& mapping = dynamic_texture_mappings[mapping_index_];
    mapping.spritesheet_offset_ = spritesheet_offset;
}



static ObjectPool<PooledRcControlBlock<Platform::DynamicTexture,
                                       Platform::dynamic_texture_count>,
                  Platform::dynamic_texture_count>
    dynamic_texture_pool("dynamic-texture-pool");



Optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    auto finalizer =
        [](PooledRcControlBlock<DynamicTexture, dynamic_texture_count>* ctrl) {
            auto& mapping =
                dynamic_texture_mappings[ctrl->data_.mapping_index()];
            mapping.reserved_ = false;
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



#ifdef __APPLE__
#include <pwd.h>
#include <sys/stat.h>
#include <unistd.h>

std::string get_save_file_path()
{
    static std::string save_path;

    if (save_path.empty()) {
        // Get user's home directory
        const char* home = getenv("HOME");
        if (!home) {
            struct passwd* pw = getpwuid(getuid());
            home = pw->pw_dir;
        }

        // Use Application Support directory (standard macOS location)
        std::string app_support =
            std::string(home) + "/Library/Application Support/Skyland";

        // Create directory if it doesn't exist
        mkdir(app_support.c_str(), 0755);

        save_path = app_support + "/save.dat";

        info(format("Save file location: %", save_path.c_str()));
    }

    return save_path;
}
#else
std::string get_save_file_path()
{
    return "save.dat"; // Current directory for Linux/Windows
}
#endif



int save_capacity = 32000;



u8 save_buffer[32000];



int Platform::save_capacity()
{
    return ::save_capacity;
}



void Platform::erase_save_sector()
{
    memset(save_buffer, 0xff, sizeof save_buffer);
}



bool Platform::write_save_data(const void* data, u32 length, u32 offset)
{
    memcpy(save_buffer + offset, data, length);

    std::ofstream out(get_save_file_path(),
                      std::ios_base::out | std::ios_base::binary);

    out.write((const char*)save_buffer, ::save_capacity);

    return true;
}



bool Platform::read_save_data(void* buffer, u32 data_length, u32 offset)
{
    memcpy(buffer, save_buffer + offset, data_length);

    std::ifstream in(get_save_file_path(),
                     std::ios_base::in | std::ios_base::binary);

    if (!in) {
        return true;
    }

    in.read((char*)save_buffer, ::save_capacity);

    return true;
}



Platform::TilePixels Platform::extract_tile(Layer layer, u16 tile)
{
    TilePixels result;
    // Initialize to zeros
    memset(result.data_, 0, sizeof(result.data_));

    SDL_Surface* surface = nullptr;
    PngPalette* palette = nullptr;
    int tile_size = 8;

    // Determine which surface and palette to use based on layer
    switch (layer) {
    case Layer::map_1_ext:
    case Layer::map_1:
        surface = tile1_surface;
        palette = &tile1_transformed_palette;
        tile_size = 16; // 16x16 tiles for map layers
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
        surface = tile0_surface;
        palette = &tile0_transformed_palette;
        tile_size = 16; // 16x16 tiles for map layers
        break;

    case Layer::overlay:
        surface = overlay_surface;
        palette = &overlay_transformed_palette;
        tile_size = 8; // 8x8 tiles for overlay
        break;

    case Layer::background:
        surface = background_surface;
        palette = &background_transformed_palette;
        tile_size = 8;
        break;

    default:
        error("extract_tile: unknown layer");
        return result;
    }

    if (!surface) {
        error("extract_tile: surface not initialized");
        return result;
    }

    if (!palette || !palette->found) {
        error("extract_tile: palette not available");
        return result;
    }

    // Calculate tile position in the surface
    const int tiles_per_row = surface->w / tile_size;
    int tile_x = tile % tiles_per_row;
    int tile_y = tile / tiles_per_row;

    // Check bounds
    if (tile_x * tile_size >= surface->w || tile_y * tile_size >= surface->h) {
        error(format("extract_tile: tile index % out of bounds", tile));
        return result;
    }

    // Build reverse lookup: RGB -> palette index
    std::map<u32, u8> rgb_to_index;
    for (int i = 0; i < palette->count; i++) {
        u32 key = (palette->colors[i].r << 16) | (palette->colors[i].g << 8) |
                  palette->colors[i].b;
        rgb_to_index[key] = i;
    }

    // Lock surface for pixel access
    if (SDL_MUSTLOCK(surface)) {
        if (SDL_LockSurface(surface) != 0) {
            error(format("Failed to lock surface: %", SDL_GetError()));
            return result;
        }
    }

    Uint8* pixels = (Uint8*)surface->pixels;
    int pitch = surface->pitch;
    int bpp = surface->format->BytesPerPixel;

    // Extract pixels and convert to palette indices
    for (int y = 0; y < tile_size && y < 16; y++) {
        for (int x = 0; x < tile_size && x < 16; x++) {
            int pixel_x = tile_x * tile_size + x;
            int pixel_y = tile_y * tile_size + y;

            // Read pixel
            Uint8* pixel_ptr = pixels + pixel_y * pitch + pixel_x * bpp;
            Uint32 pixel_value = *(Uint32*)pixel_ptr;

            // Convert to RGBA
            Uint8 r, g, b, a;
            SDL_GetRGBA(pixel_value, surface->format, &r, &g, &b, &a);

            // Look up palette index
            u8 palette_index = 0;
            if (a > 0) { // Non-transparent pixel
                u32 key = (r << 16) | (g << 8) | b;
                auto it = rgb_to_index.find(key);
                if (it != rgb_to_index.end()) {
                    palette_index = it->second;
                } else {
                    // Color not in palette - find closest match or use 0
                    warning(format(
                        "extract_tile: color not in palette at (%,%)", x, y));
                    palette_index = 0;
                }
            }

            // Store palette index
            result.data_[x][y] = palette_index;
        }
    }

    if (SDL_MUSTLOCK(surface)) {
        SDL_UnlockSurface(surface);
    }

    return result;
}



Platform::EncodedTile Platform::encode_tile(u8 tile_data[16][16])
{
    EncodedTile t;

    // Pack palette indices into 4bpp format (2 pixels per byte)
    // Match GBA's encoding order: process in 8x8 quadrants
    // tile_data is indexed as [x][y]

    u8* out = t.bytes_;

    // Top-left 8x8 quadrant
    for (int i = 0; i < 8; ++i) {     // y
        for (int j = 0; j < 8; ++j) { // x
            if (j % 2 == 0) {
                *out = tile_data[j][i] & 0x0F; // Low nibble
            } else {
                *out |= (tile_data[j][i] & 0x0F) << 4; // High nibble
                out++;
            }
        }
    }

    // Top-right 8x8 quadrant
    for (int i = 0; i < 8; ++i) {      // y
        for (int j = 8; j < 16; ++j) { // x
            if (j % 2 == 0) {
                *out = tile_data[j][i] & 0x0F;
            } else {
                *out |= (tile_data[j][i] & 0x0F) << 4;
                out++;
            }
        }
    }

    // Bottom-left 8x8 quadrant
    for (int i = 8; i < 16; ++i) {    // y
        for (int j = 0; j < 8; ++j) { // x
            if (j % 2 == 0) {
                *out = tile_data[j][i] & 0x0F;
            } else {
                *out |= (tile_data[j][i] & 0x0F) << 4;
                out++;
            }
        }
    }

    // Bottom-right 8x8 quadrant
    for (int i = 8; i < 16; ++i) {     // y
        for (int j = 8; j < 16; ++j) { // x
            if (j % 2 == 0) {
                *out = tile_data[j][i] & 0x0F;
            } else {
                *out |= (tile_data[j][i] & 0x0F) << 4;
                out++;
            }
        }
    }

    return t;
}



Platform& Platform::instance()
{
    return *__platform__;
}



void Platform::set_palette(Layer layer, u16 x, u16 y, u16 palette)
{
    auto it = tile_layers_[layer].find({x, y});
    if (it != tile_layers_[layer].end()) {
        it->second.palette = palette;
    }
}



u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    auto it = tile_layers_[layer].find({x, y});
    if (it != tile_layers_[layer].end()) {
        return it->second.palette;
    }
    return 0;
}



void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
    if (layer == Layer::map_0 and tile0_surface and tile0_surface->h == 16) {
        // We need to metatile...
        // The tilesheet is arranged in 2x2 metatiles:
        //   0 1 4 5 8 9 ...
        //   2 3 6 7 10 11 ...
        // So tile index 5 is at metatile 1, position top-right (x=3, y=0)

        u16 metatile_index = val / 4;  // Which 2x2 metatile group
        u16 within_metatile = val % 4; // Position within the metatile (0-3)

        u16 metatile_x = metatile_index * 2; // Each metatile is 2 tiles wide
        u16 tile_x =
            metatile_x + (within_metatile & 1); // Add 0 or 1 for left/right
        u16 tile_y =
            (within_metatile & 2) >> 1; // 0 for top row, 1 for bottom row

        TileDesc target = tile_y * (tile0_surface->w / 8) + tile_x;

        set_tile(layer, x, y, target);

    } else {
        set_tile(layer, x, y, val);
    }
}


void Platform::blit_t0_erase(u16 index)
{
}



void Platform::blit_t1_erase(u16 index)
{
}



void Platform::blit_t0_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
}



void Platform::blit_t1_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
}



#include <filesystem>



void Platform::walk_filesystem(
    Function<8 * sizeof(void*), void(const char* path)> callback)
{
    std::vector<std::string> paths;

    auto res_path = resource_path();
    using recursive_directory_iterator =
        std::filesystem::recursive_directory_iterator;
    for (const auto& dirent : recursive_directory_iterator(res_path)) {
        if (dirent.is_regular_file()) {
            auto full_path = dirent.path().string();
            if (full_path.size() and full_path[full_path.size() - 1] == '~') {
                // Gah! It's an emacs temporary file...
                continue;
            }
            // Remove resource_path() prefix to get relative path
            if (full_path.size() > res_path.size() &&
                full_path.substr(0, res_path.size()) == res_path) {
                auto relative_path = full_path.substr(res_path.size());
                // Remove leading path delimiter if present
                if (!relative_path.empty() &&
                    (relative_path[0] == '/' || relative_path[0] == '\\')) {
                    relative_path = relative_path.substr(1);
                }
                relative_path = "/" + relative_path;
                paths.push_back(relative_path);
            }
        }
    }
    std::sort(paths.begin(), paths.end());
    for (auto& path : paths) {
        callback(path.c_str());
    }
}



struct GlyphCacheEntry
{
    std::string texture_name;
    u16 offset_in_texture;
    TileDesc vram_tile_index;
    bool in_use;
};

static std::vector<GlyphCacheEntry> glyph_cache;
static const int max_glyph_cache_size = 256;



bool is_glyph(TileDesc td)
{
    return td >= overlay_base_tile_count;
}



static SDL_Surface* load_charset_surface(const char* texture_name)
{
    auto it = charset_surfaces.find(texture_name);
    if (it != charset_surfaces.end()) {
        return it->second;
    }

    std::string full_path =
        resource_path() + "images" + PATH_DELIMITER + texture_name + ".png";

    SDL_Surface* surface = IMG_Load(full_path.c_str());
    if (!surface) {
        error(format(
            "Failed to load charset %: %", full_path.c_str(), IMG_GetError()));
        return nullptr;
    }

    // Set black as transparent
    Uint32 color_key = SDL_MapRGB(surface->format, 0, 0, 16);
    SDL_SetColorKey(surface, SDL_TRUE, color_key);

    charset_surfaces[texture_name] = surface;

    return surface;
}



static void copy_tile_to_surface(SDL_Surface* dst,
                                 SDL_Surface* src,
                                 int dst_tile_x,
                                 int dst_tile_y,
                                 int src_tile_x,
                                 int src_tile_y)
{
    SDL_Rect src_rect;
    src_rect.x = src_tile_x * 8;
    src_rect.y = src_tile_y * 8;
    src_rect.w = 8;
    src_rect.h = 8;

    SDL_Rect dst_rect;
    dst_rect.x = dst_tile_x * 8;
    dst_rect.y = dst_tile_y * 8;
    dst_rect.w = 8;
    dst_rect.h = 8;

    SDL_FillRect(dst, &dst_rect, 0x000000);
    SDL_BlitSurface(src, &src_rect, dst, &dst_rect);
}



static void expand_overlay_surface()
{
    if (not renderer) {
        return;
    }

    if (!overlay_surface) {
        return;
    }

    int old_width = overlay_surface->w;
    int old_height = overlay_surface->h;

    // Expand vertically (add a new row of 8 pixel height)
    int new_height = old_height + 8;

    SDL_Surface* new_surface =
        SDL_CreateRGBSurface(0,
                             old_width,
                             new_height,
                             overlay_surface->format->BitsPerPixel,
                             overlay_surface->format->Rmask,
                             overlay_surface->format->Gmask,
                             overlay_surface->format->Bmask,
                             overlay_surface->format->Amask);

    if (!new_surface) {
        error(format("Failed to expand overlay surface: %", SDL_GetError()));
        return;
    }

    // Copy old surface to new surface
    SDL_BlitSurface(overlay_surface, nullptr, new_surface, nullptr);

    // Set color key on new surface
    Uint32 color_key = SDL_MapRGB(new_surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(new_surface, SDL_TRUE, color_key);

    // Replace old surface
    SDL_FreeSurface(overlay_surface);
    overlay_surface = new_surface;

    // Recreate texture from expanded surface
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
    }

    current_overlay_texture =
        SDL_CreateTextureFromSurface(renderer, overlay_surface);
    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);

    overlay_texture_width = overlay_surface->w;
    overlay_texture_height = overlay_surface->h;
}



TileDesc Platform::map_glyph(const utf8::Codepoint& glyph,
                             const TextureMapping& mapping_info)
{
    if (not renderer) {
        return 495;
    }

    if (not get_gflag(GlobalFlag::glyph_mode)) {
        return 495; // bad_glyph equivalent
    }

    // Check if this glyph is already mapped
    for (auto& entry : glyph_cache) {
        if (entry.in_use && entry.texture_name == mapping_info.texture_name_ &&
            entry.offset_in_texture == mapping_info.offset_) {
            return entry.vram_tile_index;
        }
    }

    int slot_index = -1;
    for (size_t i = 0; i < glyph_cache.size(); ++i) {
        if (!glyph_cache[i].in_use) {
            slot_index = i;
            break;
        }
    }

    if (slot_index == -1) {
        if (glyph_cache.size() >= max_glyph_cache_size) {
            // Cache full - could implement LRU eviction here
            warning("Glyph cache full!");
            return 495; // bad_glyph
        }

        slot_index = glyph_cache.size();
        glyph_cache.push_back(GlyphCacheEntry{});
    }

    // Load the charset texture
    SDL_Surface* charset = load_charset_surface(mapping_info.texture_name_);
    if (!charset) {
        return 495; // bad_glyph
    }

    // Calculate the tile index in the expanded overlay texture
    // We append new glyphs after the base overlay tiles
    TileDesc new_tile_index = overlay_base_tile_count + slot_index;

    // Check if we need to expand the overlay surface
    int tiles_per_row = overlay_texture_width / 8;
    int required_tiles = new_tile_index + 1;
    int current_tiles =
        (overlay_texture_width / 8) * (overlay_texture_height / 8);

    while (current_tiles < required_tiles) {
        expand_overlay_surface();
        current_tiles =
            (overlay_texture_width / 8) * (overlay_texture_height / 8);
    }

    // Calculate position in overlay surface where this glyph will go
    int dst_tile_x = new_tile_index % tiles_per_row;
    int dst_tile_y = new_tile_index / tiles_per_row;

    // Calculate position in charset surface where the glyph is
    int charset_tiles_per_row = charset->w / 8;
    int src_tile_x = mapping_info.offset_ % charset_tiles_per_row;
    int src_tile_y = mapping_info.offset_ / charset_tiles_per_row;

    // Copy the 8x8 glyph from charset to overlay surface
    copy_tile_to_surface(overlay_surface,
                         charset,
                         dst_tile_x,
                         dst_tile_y,
                         src_tile_x,
                         src_tile_y);

    // Recreate the texture from the modified surface
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
    }
    current_overlay_texture =
        SDL_CreateTextureFromSurface(renderer, overlay_surface);
    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);

    // Update cache entry
    glyph_cache[slot_index] = {
        mapping_info.texture_name_, mapping_info.offset_, new_tile_index, true};

    return new_tile_index;
}



void Platform::set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors)
{
    set_tile(Layer::overlay, x, y, glyph);

    auto& val = tile_layers_[Layer::overlay][{x, y}];
    val.text_fg_color_ = colors.foreground_;
    val.text_bg_color_ = colors.background_;
}



void Platform::enable_glyph_mode(bool enabled)
{
    set_gflag(GlobalFlag::glyph_mode, enabled);

    if (enabled) {
        for (auto& entry : glyph_cache) {
            entry.in_use = false;
        }
    }
}



void Platform::fill_overlay(u16 tile_desc)
{
    // When filling overlay, mark all dynamic glyphs as unused
    for (auto& entry : glyph_cache) {
        entry.in_use = false;
    }

    // Fill all 32x32 overlay tiles with the specified tile
    for (u16 y = 0; y < 32; ++y) {
        for (u16 x = 0; x < 32; ++x) {
            tile_layers_[Layer::overlay][{x, y}] = {tile_desc, 0};
        }
    }
}



void cleanup_charset_surfaces()
{
    for (auto& [name, surface] : charset_surfaces) {
        SDL_FreeSurface(surface);
    }
    charset_surfaces.clear();

    for (auto& [name, surface] : overlay_source_cache) {
        SDL_FreeSurface(surface);
    }
    overlay_source_cache.clear();
}



void Platform::override_priority(Layer layer, int priority)
{
    // TODO...
}



void Platform::overwrite_t0_tile(u16 index, const EncodedTile& t)
{
    if (not renderer) {
        return;
    }

    if (!tile0_surface) {
        error("overwrite_t0_tile: tile0_surface not initialized");
        return;
    }

    if (!tile0_transformed_palette.found) {
        error("overwrite_t0_tile: No tile0 palette loaded");
        return;
    }

    // Tile0 tiles are 16x16 pixels (use all four 8x8 quadrants)
    const int tile_size = 16;
    const int tiles_per_row = tile0_surface->w / tile_size;

    int tile_x = index % tiles_per_row;
    int tile_y = index / tiles_per_row;

    // Check bounds
    if (tile_x * tile_size >= tile0_surface->w ||
        tile_y * tile_size >= tile0_surface->h) {
        error(format("overwrite_t0_tile: index % out of bounds", index));
        return;
    }

    // Lock the surface for pixel access
    if (SDL_MUSTLOCK(tile0_surface)) {
        if (SDL_LockSurface(tile0_surface) != 0) {
            error(format("Failed to lock tile0 surface: %", SDL_GetError()));
            return;
        }
    }

    Uint8* pixels = (Uint8*)tile0_surface->pixels;
    int pitch = tile0_surface->pitch;
    int bpp = tile0_surface->format->BytesPerPixel;

    // Unpack the 4bpp data for all four 8x8 quadrants
    const u8* src = t.bytes_;

    for (int y = 0; y < tile_size; y++) {
        for (int x = 0; x < tile_size; x++) {
            int pixel_x = tile_x * tile_size + x;
            int pixel_y = tile_y * tile_size + y;

            // Calculate which quadrant and position within quadrant
            int quadrant_x = x / 8; // 0 or 1
            int quadrant_y = y / 8; // 0 or 1
            int local_x = x % 8;
            int local_y = y % 8;

            // Each quadrant is 32 bytes (8x8 pixels, 2 pixels per byte)
            int quadrant_offset = (quadrant_y * 2 + quadrant_x) * 32;
            int byte_offset = quadrant_offset + (local_y * 4) + (local_x / 2);

            // Extract the 4-bit palette index
            u8 palette_index;
            if (local_x % 2 == 0) {
                palette_index = src[byte_offset] & 0x0F;
            } else {
                palette_index = (src[byte_offset] >> 4) & 0x0F;
            }

            // Look up the color in the palette
            SDL_Color color;
            if (palette_index >= tile0_transformed_palette.count or
                palette_index == 0) {
                color = {0xFF, 0x00, 0xFF, 0x00};
            } else {
                color = tile0_transformed_palette.colors[palette_index];
                if (color.a != 0) {
                    color.a = 255;
                }
            }

            // Write the pixel to the surface
            Uint32 pixel_value = SDL_MapRGBA(
                tile0_surface->format, color.r, color.g, color.b, color.a);

            Uint8* pixel_ptr = pixels + pixel_y * pitch + pixel_x * bpp;
            *(Uint32*)pixel_ptr = pixel_value;
        }
    }

    if (SDL_MUSTLOCK(tile0_surface)) {
        SDL_UnlockSurface(tile0_surface);
    }

    // Recreate the texture from the modified surface
    if (tile0_texture) {
        SDL_DestroyTexture(tile0_texture);
    }
    tile0_texture = SDL_CreateTextureFromSurface(renderer, tile0_surface);
    if (!tile0_texture) {
        error(format("Failed to recreate tile0 texture: %", SDL_GetError()));
        return;
    }
    SDL_SetTextureBlendMode(tile0_texture, SDL_BLENDMODE_BLEND);
}



void Platform::overwrite_t1_tile(u16 index, const EncodedTile& t)
{
    if (not renderer) {
        return;
    }

    if (!tile1_surface) {
        error("overwrite_t1_tile: tile1_surface not initialized");
        return;
    }

    if (!tile1_transformed_palette.found) {
        error("overwrite_t1_tile: No tile1 palette loaded");
        return;
    }

    // Tile1 tiles are 16x16 pixels (use all four 8x8 quadrants)
    const int tile_size = 16;
    const int tiles_per_row = tile1_surface->w / tile_size;

    int tile_x = index % tiles_per_row;
    int tile_y = index / tiles_per_row;

    // Check bounds
    if (tile_x * tile_size >= tile1_surface->w ||
        tile_y * tile_size >= tile1_surface->h) {
        error(format("overwrite_t1_tile: index % out of bounds", index));
        return;
    }

    // Lock the surface for pixel access
    if (SDL_MUSTLOCK(tile1_surface)) {
        if (SDL_LockSurface(tile1_surface) != 0) {
            error(format("Failed to lock tile1 surface: %", SDL_GetError()));
            return;
        }
    }

    Uint8* pixels = (Uint8*)tile1_surface->pixels;
    int pitch = tile1_surface->pitch;
    int bpp = tile1_surface->format->BytesPerPixel;

    // Unpack the 4bpp data for all four 8x8 quadrants
    const u8* src = t.bytes_;

    for (int y = 0; y < tile_size; y++) {
        for (int x = 0; x < tile_size; x++) {
            int pixel_x = tile_x * tile_size + x;
            int pixel_y = tile_y * tile_size + y;

            // Calculate which quadrant and position within quadrant
            int quadrant_x = x / 8; // 0 or 1
            int quadrant_y = y / 8; // 0 or 1
            int local_x = x % 8;
            int local_y = y % 8;

            // Each quadrant is 32 bytes (8x8 pixels, 2 pixels per byte)
            int quadrant_offset = (quadrant_y * 2 + quadrant_x) * 32;
            int byte_offset = quadrant_offset + (local_y * 4) + (local_x / 2);

            // Extract the 4-bit palette index
            u8 palette_index;
            if (local_x % 2 == 0) {
                palette_index = src[byte_offset] & 0x0F;
            } else {
                palette_index = (src[byte_offset] >> 4) & 0x0F;
            }

            // Look up the color in the palette
            SDL_Color color;
            if (palette_index >= tile1_transformed_palette.count or
                palette_index == 0) {
                color = {0xFF, 0x00, 0xFF, 0x00};
            } else {
                color = tile1_transformed_palette.colors[palette_index];
                if (color.a != 0) {
                    color.a = 255;
                }
            }

            // Write the pixel to the surface
            Uint32 pixel_value = SDL_MapRGBA(
                tile1_surface->format, color.r, color.g, color.b, color.a);

            Uint8* pixel_ptr = pixels + pixel_y * pitch + pixel_x * bpp;
            *(Uint32*)pixel_ptr = pixel_value;
        }
    }

    if (SDL_MUSTLOCK(tile1_surface)) {
        SDL_UnlockSurface(tile1_surface);
    }

    // Recreate the texture from the modified surface
    if (tile1_texture) {
        SDL_DestroyTexture(tile1_texture);
    }
    tile1_texture = SDL_CreateTextureFromSurface(renderer, tile1_surface);
    if (!tile1_texture) {
        error(format("Failed to recreate tile1 texture: %", SDL_GetError()));
        return;
    }
    SDL_SetTextureBlendMode(tile1_texture, SDL_BLENDMODE_BLEND);
}



void Platform::overwrite_overlay_tile(u16 index, const EncodedTile& t)
{
    if (not renderer) {
        return;
    }

    if (!overlay_surface) {
        error("overwrite_overlay_tile: overlay_surface not initialized");
        return;
    }

    if (!overlay_transformed_palette.found) {
        error("overwrite_overlay_tile: No overlay palette loaded");
        return;
    }

    // Overlay tiles are 8x8 pixels, so we only use the top-left quadrant
    // of the 16x16 encoded tile (first 32 bytes = 64 pixels in 4bpp)
    const int tile_size = 8;
    const int tiles_per_row = overlay_surface->w / tile_size;

    int tile_x = index % tiles_per_row;
    int tile_y = index / tiles_per_row;

    // Check bounds
    if (tile_x * tile_size >= overlay_surface->w ||
        tile_y * tile_size >= overlay_surface->h) {
        error(format("overwrite_overlay_tile: index % out of bounds", index));
        return;
    }

    // Lock the surface for pixel access
    if (SDL_MUSTLOCK(overlay_surface)) {
        if (SDL_LockSurface(overlay_surface) != 0) {
            error(format("Failed to lock overlay surface: %", SDL_GetError()));
            return;
        }
    }

    Uint8* pixels = (Uint8*)overlay_surface->pixels;
    int pitch = overlay_surface->pitch;
    int bpp = overlay_surface->format->BytesPerPixel;

    // Unpack the 4bpp data from the top-left 8x8 quadrant
    // The first 32 bytes contain 64 pixels (2 pixels per byte)
    const u8* src = t.bytes_;

    for (int y = 0; y < tile_size; y++) {
        for (int x = 0; x < tile_size; x++) {
            int pixel_x = tile_x * tile_size + x;
            int pixel_y = tile_y * tile_size + y;

            // Calculate byte offset in the packed data
            // 8 pixels per row, 2 pixels per byte = 4 bytes per row
            int byte_offset = (y * 4) + (x / 2);

            // Extract the 4-bit palette index
            u8 palette_index;
            if (x % 2 == 0) {
                palette_index = src[byte_offset] & 0x0F; // Low nibble
            } else {
                palette_index = (src[byte_offset] >> 4) & 0x0F; // High nibble
            }

            // Look up the color in the palette
            SDL_Color color;
            if (palette_index >= overlay_transformed_palette.count or
                palette_index == 0) {
                // Out of bounds - use transparent magenta
                color = {0xFF, 0x00, 0xFF, 0x00};
            } else {
                color = overlay_transformed_palette.colors[palette_index];
                // Force fully opaque unless already transparent
                if (color.a != 0) {
                    color.a = 255;
                }
            }

            // Write the pixel to the surface
            Uint32 pixel_value = SDL_MapRGBA(
                overlay_surface->format, color.r, color.g, color.b, color.a);

            Uint8* pixel_ptr = pixels + pixel_y * pitch + pixel_x * bpp;
            *(Uint32*)pixel_ptr = pixel_value;
        }
    }

    if (SDL_MUSTLOCK(overlay_surface)) {
        SDL_UnlockSurface(overlay_surface);
    }

    // Recreate the texture from the modified surface
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
    }
    current_overlay_texture =
        SDL_CreateTextureFromSurface(renderer, overlay_surface);
    if (!current_overlay_texture) {
        error(format("Failed to recreate overlay texture: %", SDL_GetError()));
        return;
    }
    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);
}



#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wall"
#pragma GCC diagnostic ignored "-Wextra"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wdouble-promotion"
#endif

#define STB_IMAGE_IMPLEMENTATION
#include "stb/stb_image.h"

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif


static Platform::Screen::Shader current_shader = passthrough_shader;
static int current_shader_arg = 0;


struct DynamicPalette
{
    std::map<u32, u8> color_to_index; // RGB value -> palette index
    SDL_Color colors[256];            // Up to 256 unique colors
    u8 count = 0;

    u8 add_color(const SDL_Color& c)
    {
        u32 key = (c.r << 16) | (c.g << 8) | c.b;

        auto it = color_to_index.find(key);
        if (it != color_to_index.end()) {
            return it->second; // Already in palette
        }

        if (count >= 255) {
            warning("Palette overflow - reusing index 0");
            return 0;
        }

        u8 index = count++;
        color_to_index[key] = index;
        colors[index] = c;
        return index;
    }
};



static PngPalette extract_png_palette(const std::string& path)
{
    PngPalette result;

    std::ifstream file(path, std::ios::binary);
    if (!file) {
        error(format("Failed to open % for palette extraction", path.c_str()));
        return result;
    }

    // Read and verify PNG signature
    unsigned char signature[8];
    file.read((char*)signature, 8);

    // PNG signature: 137 80 78 71 13 10 26 10
    const unsigned char png_sig[8] = {137, 80, 78, 71, 13, 10, 26, 10};
    if (memcmp(signature, png_sig, 8) != 0) {
        error(format("% is not a valid PNG file", path.c_str()));
        return result;
    }

    // Read chunks until we find PLTE
    while (file.good()) {
        // Read chunk length (4 bytes, big-endian)
        unsigned char length_bytes[4];
        file.read((char*)length_bytes, 4);
        if (!file.good())
            break;

        uint32_t chunk_length = (length_bytes[0] << 24) |
                                (length_bytes[1] << 16) |
                                (length_bytes[2] << 8) | length_bytes[3];

        // Read chunk type (4 bytes)
        char chunk_type[5] = {0};
        file.read(chunk_type, 4);
        if (!file.good())
            break;

        // Check if this is a PLTE chunk
        if (strncmp(chunk_type, "PLTE", 4) == 0) {
            // PLTE chunk found! Each entry is 3 bytes (RGB)
            result.count = chunk_length / 3;

            if (result.count > 256) {
                warning(format("PNG palette has % entries, clamping to 256",
                               result.count));
                result.count = 256;
            }

            for (int i = 0; i < result.count; i++) {
                unsigned char rgb[3];
                file.read((char*)rgb, 3);
                result.colors[i].r = rgb[0];
                result.colors[i].g = rgb[1];
                result.colors[i].b = rgb[2];
                result.colors[i].a = 255;
            }

            result.found = true;
            return result;
        }

        // Skip this chunk's data and CRC (chunk_length + 4 bytes for CRC)
        file.seekg(chunk_length + 4, std::ios::cur);
    }

    // No PLTE chunk found - not an indexed image
    return result;
}



#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wall"
#pragma GCC diagnostic ignored "-Wextra"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wdouble-promotion"
#endif

static SDL_Surface* load_png_with_stb(const std::string& path,
                                      const char* name,
                                      ShaderPalette shader_palette)
{
    // First, try to extract palette from PNG
    PngPalette original_palette = extract_png_palette(path);

    // Load the image with stb_image
    int width, height, channels;
    unsigned char* data =
        stbi_load(path.c_str(), &width, &height, &channels, 0);

    if (!data) {
        error(format("stb_image failed to load %: %",
                     path.c_str(),
                     stbi_failure_reason()));
        return nullptr;
    }

    SDL_Surface* surface = nullptr;

    // If we found a palette, we need to apply the shader
    if (original_palette.found) {
        // Apply shader to the original palette
        SDL_Color transformed_palette[256];
        for (int i = 0; i < original_palette.count; i++) {
            ColorConstant original =
                sdl_color_to_colorconstant(original_palette.colors[i]);
            ColorConstant transformed =
                current_shader(std::move(shader_palette),
                               std::move(original),
                               std::move(current_shader_arg),
                               std::move(i));
            transformed_palette[i] = color_to_sdl(transformed);
        }

        // Save the transformed palette for later use in encode_tile()
        PngPalette* target_palette = nullptr;
        if (shader_palette == ShaderPalette::overlay) {
            target_palette = &overlay_transformed_palette;
        } else if (shader_palette == ShaderPalette::tile0) {
            target_palette = &tile0_transformed_palette;
        } else if (shader_palette == ShaderPalette::tile1) {
            target_palette = &tile1_transformed_palette;
        } else if (shader_palette == ShaderPalette::spritesheet) {
            target_palette = &sprite_transformed_palette;
        } else if (shader_palette == ShaderPalette::background) {
            target_palette = &background_transformed_palette;
        }

        if (target_palette) {
            target_palette->count = original_palette.count;
            target_palette->found = true;
            memcpy(target_palette->colors,
                   transformed_palette,
                   sizeof(SDL_Color) * original_palette.count);
        }

        // Now we need to map the RGB values back to palette indices
        // Build a lookup map: RGB -> palette index
        std::map<u32, u8> rgb_to_index;
        for (int i = 0; i < original_palette.count; i++) {
            u32 key = (original_palette.colors[i].r << 16) |
                      (original_palette.colors[i].g << 8) |
                      original_palette.colors[i].b;
            rgb_to_index[key] = i;
        }

        // Create surface from stb_image data
        int bpp = channels * 8;
        SDL_Surface* temp_surface =
            SDL_CreateRGBSurfaceFrom(data,
                                     width,
                                     height,
                                     bpp,
                                     width * channels,
                                     0x000000FF,
                                     0x0000FF00,
                                     0x00FF0000,
                                     channels == 4 ? 0xFF000000 : 0);

        if (!temp_surface) {
            stbi_image_free(data);
            error(format("Failed to create surface: %", SDL_GetError()));
            return nullptr;
        }

        // Convert to RGBA32 for processing
        SDL_Surface* rgba_surface =
            SDL_ConvertSurfaceFormat(temp_surface, SDL_PIXELFORMAT_RGBA32, 0);
        SDL_FreeSurface(temp_surface);
        stbi_image_free(data);

        if (!rgba_surface) {
            error(format("Failed to convert to RGBA32: %", SDL_GetError()));
            return nullptr;
        }

        // Create output surface
        SDL_Surface* result_surface =
            SDL_CreateRGBSurface(0,
                                 width,
                                 height,
                                 32,
                                 rgba_surface->format->Rmask,
                                 rgba_surface->format->Gmask,
                                 rgba_surface->format->Bmask,
                                 rgba_surface->format->Amask);

        if (!result_surface) {
            error(format("Failed to create result surface: %", SDL_GetError()));
            SDL_FreeSurface(rgba_surface);
            return nullptr;
        }

        // Apply shader transformation
        if (SDL_MUSTLOCK(rgba_surface))
            SDL_LockSurface(rgba_surface);
        if (SDL_MUSTLOCK(result_surface))
            SDL_LockSurface(result_surface);

        Uint32* src_pixels = (Uint32*)rgba_surface->pixels;
        Uint32* dst_pixels = (Uint32*)result_surface->pixels;
        int pixel_count = width * height;

        for (int i = 0; i < pixel_count; i++) {
            Uint32 pixel = src_pixels[i];
            SDL_Color c;
            SDL_GetRGBA(pixel, rgba_surface->format, &c.r, &c.g, &c.b, &c.a);

            // Handle transparency
            if (c.a == 0) {
                dst_pixels[i] = 0;
                continue;
            }

            // Look up original palette index
            u32 key = (c.r << 16) | (c.g << 8) | c.b;
            auto it = rgb_to_index.find(key);

            if (it != rgb_to_index.end()) {
                // Found palette index - use shader-transformed color
                u8 index = it->second;
                SDL_Color& transformed = transformed_palette[index];

                dst_pixels[i] = SDL_MapRGBA(result_surface->format,
                                            transformed.r,
                                            transformed.g,
                                            transformed.b,
                                            c.a);
            } else {
                // Not in palette? This shouldn't happen for indexed images
                // Just pass through
                dst_pixels[i] = pixel;
            }
        }

        if (SDL_MUSTLOCK(rgba_surface))
            SDL_UnlockSurface(rgba_surface);
        if (SDL_MUSTLOCK(result_surface))
            SDL_UnlockSurface(result_surface);

        SDL_FreeSurface(rgba_surface);

        // Set magenta as color key
        Uint32 magenta = SDL_MapRGB(result_surface->format, 0xFF, 0x00, 0xFF);
        SDL_SetColorKey(result_surface, SDL_TRUE, magenta);

        return result_surface;

    } else {
        int bpp = channels * 8;
        surface = SDL_CreateRGBSurfaceFrom(data,
                                           width,
                                           height,
                                           bpp,
                                           width * channels,
                                           0x000000FF,
                                           0x0000FF00,
                                           0x00FF0000,
                                           channels == 4 ? 0xFF000000 : 0);

        if (!surface) {
            stbi_image_free(data);
            error(format("Failed to create surface: %", SDL_GetError()));
            return nullptr;
        }

        SDL_Surface* copy = SDL_ConvertSurface(surface, surface->format, 0);
        SDL_FreeSurface(surface);
        stbi_image_free(data);

        if (copy) {
            Uint32 magenta = SDL_MapRGB(copy->format, 0xFF, 0x00, 0xFF);
            SDL_SetColorKey(copy, SDL_TRUE, magenta);
        }

        return copy;
    }
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif



void load_metatiled_chunk(SDL_Surface* source_surface,
                          int meta_tile_size,
                          int tiles_per_meta,
                          int tiles_per_strip,
                          int strips_per_meta,
                          int copy_count,
                          TileDesc src,
                          TileDesc dst)
{
    const int tile_size = 8;
    const int meta_tiles_per_row = source_surface->w / meta_tile_size;

    int num_meta_tiles = copy_count / tiles_per_meta;

    for (u16 i = 0; i < num_meta_tiles; ++i) {
        TileDesc src_tile_index = src + (i * tiles_per_meta);

        int meta_tile_index = src_tile_index / tiles_per_meta;

        // Find which meta-tile block in the source texture
        int src_meta_x = meta_tile_index % meta_tiles_per_row;
        int src_meta_y = meta_tile_index / meta_tiles_per_row;

        // Base pixel position of this meta-tile block
        int base_src_x = src_meta_x * meta_tile_size;
        int base_src_y = src_meta_y * meta_tile_size;

        // Destination starts at dst + (i * tiles_per_meta)
        TileDesc dst_base = dst + (i * tiles_per_meta);

        // Copy horizontal strips
        for (int strip = 0; strip < strips_per_meta; ++strip) {
            // Copy tiles in this strip
            for (int tile_in_strip = 0; tile_in_strip < tiles_per_strip;
                 ++tile_in_strip) {
                // Source: this 8x8 tile within the meta-tile block
                SDL_Rect src_rect;
                src_rect.x = base_src_x + (tile_in_strip * tile_size);
                src_rect.y = base_src_y + (strip * tile_size);
                src_rect.w = tile_size;
                src_rect.h = tile_size;

                // Destination: linear placement (always y=0)
                int linear_tile_index = strip * tiles_per_strip + tile_in_strip;
                TileDesc dst_tile = dst_base + linear_tile_index;

                SDL_Rect dst_rect;
                dst_rect.x = dst_tile * tile_size;
                dst_rect.y = 0;
                dst_rect.w = tile_size;
                dst_rect.h = tile_size;

                Uint32 transparent =
                    SDL_MapRGBA(overlay_surface->format, 255, 0, 255, 0);
                SDL_FillRect(overlay_surface, &dst_rect, transparent);

                if (SDL_BlitSurface(source_surface,
                                    &src_rect,
                                    overlay_surface,
                                    &dst_rect) != 0) {
                    error(format(
                        "load_overlay_chunk: Failed to blit strip %/tile %: %",
                        strip,
                        tile_in_strip,
                        SDL_GetError()));
                }
            }
        }
    }
}



static int FIXME_get_metatile_size(const char* texture_name)
{
    if (str_eq(texture_name, "character_art") or
        str_eq(texture_name, "appendix")) {
        return 4;
    } else if (str_eq(texture_name, "paint_icons")) {
        return 2;
    } else {
        return 1;
    }
}



StringBuffer<256> last_overlay_texture;
void Platform::load_overlay_chunk(TileDesc dst,
                                  TileDesc src,
                                  u16 count,
                                  const char* image_file)
{
    if (not renderer) {
        return;
    }

    if (!overlay_surface) {
        error("load_overlay_chunk: overlay_surface not initialized");
        return;
    }

    SDL_Surface* source_surface = nullptr;
    std::string cache_key;

    if (image_file) {
        // Load the specified image file
        cache_key = image_file;

        // Check cache first
        auto it = overlay_source_cache.find(cache_key);
        if (it != overlay_source_cache.end()) {
            source_surface = it->second;
        } else {
            std::string full_path = resource_path() + "images" +
                                    PATH_DELIMITER + image_file + ".png";

            source_surface = load_png_with_stb(
                full_path, image_file, ShaderPalette::overlay);
            if (!source_surface) {
                error(
                    format("load_overlay_chunk: Failed to load %", image_file));
                return;
            }

            // Cache it
            overlay_source_cache[cache_key] = source_surface;
        }
    } else {
        // Use current overlay texture - need to reload the full uncropped version
        if (last_overlay_texture.empty()) {
            error("load_overlay_chunk: No current overlay texture");
            return;
        }

        cache_key = last_overlay_texture.c_str();

        // Check cache first
        auto it = overlay_source_cache.find(cache_key);
        if (it != overlay_source_cache.end()) {
            source_surface = it->second;
        } else {
            std::string full_path = resource_path() + "images" +
                                    PATH_DELIMITER + cache_key + ".png";

            source_surface = load_png_with_stb(
                full_path, cache_key.c_str(), ShaderPalette::overlay);
            if (!source_surface) {
                error(format(
                    "load_overlay_chunk: Failed to reload current texture %",
                    cache_key.c_str()));
                return;
            }

            // Cache it
            overlay_source_cache[cache_key] = source_surface;
        }
    }

    const int tile_size = 8;
    const int tiles_per_row_dst = overlay_surface->w / tile_size;

    auto metatile_size = FIXME_get_metatile_size(image_file ? image_file : "");

    if (metatile_size > 1) {
        // Meta-tile mode: source has 32x32 pixel blocks, each containing 16 8x8 tiles
        // src is a TILE index (8x8), not a meta-tile index
        // Each 32x32 meta-tile = 16 8x8 tiles
        load_metatiled_chunk(source_surface,
                             metatile_size * 8,
                             metatile_size * metatile_size,
                             metatile_size,
                             metatile_size,
                             count,
                             src,
                             dst);
    } else {
        // Regular 8x8 tile mode
        const int tiles_per_row_src = source_surface->w / tile_size;

        for (u16 i = 0; i < count; ++i) {
            TileDesc src_tile = src + i;
            TileDesc dst_tile = dst + i;

            // Calculate source tile coordinates
            int src_tile_x = src_tile % tiles_per_row_src;
            int src_tile_y = src_tile / tiles_per_row_src;

            // Calculate destination tile coordinates
            int dst_tile_x = dst_tile % tiles_per_row_dst;
            int dst_tile_y = dst_tile / tiles_per_row_dst;

            SDL_Rect src_rect;
            src_rect.x = src_tile_x * tile_size;
            src_rect.y = src_tile_y * tile_size;
            src_rect.w = tile_size;
            src_rect.h = tile_size;

            SDL_Rect dst_rect;
            dst_rect.x = dst_tile_x * tile_size;
            dst_rect.y = dst_tile_y * tile_size;
            dst_rect.w = tile_size;
            dst_rect.h = tile_size;

            Uint32 transparent =
                SDL_MapRGBA(overlay_surface->format, 255, 0, 255, 0);
            SDL_FillRect(overlay_surface, &dst_rect, transparent);

            // Blit the tile
            if (SDL_BlitSurface(
                    source_surface, &src_rect, overlay_surface, &dst_rect) !=
                0) {
                error(format("load_overlay_chunk: Failed to blit tile %: %",
                             i,
                             SDL_GetError()));
            }
        }
    }


    // Recreate the texture from the modified surface
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
    }

    current_overlay_texture =
        SDL_CreateTextureFromSurface(renderer, overlay_surface);
    if (!current_overlay_texture) {
        error(format("load_overlay_chunk: Failed to recreate texture: %",
                     SDL_GetError()));
        return;
    }

    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);
}



static Vec2<s32> tile0_scroll;
static Vec2<s32> tile1_scroll;
Vec2<Float> overlay_origin;

static bool tile0_index_zero_is_transparent = true;
static bool tile1_index_zero_is_transparent = true;
static bool overlay_index_zero_is_transparent = true;



static bool is_tile_transparent(SDL_Surface* surface,
                                int tile_x,
                                int tile_y,
                                int tile_size = 8)
{
    if (!surface) {
        return true;
    }

    if (SDL_MUSTLOCK(surface)) {
        if (SDL_LockSurface(surface) != 0) {
            error(format("Failed to lock surface: %", SDL_GetError()));
            return true;
        }
    }

    bool is_transparent = true;
    Uint8* pixels = (Uint8*)surface->pixels;
    int pitch = surface->pitch;
    int bpp = surface->format->BytesPerPixel;

    for (int y = 0; y < tile_size && is_transparent; ++y) {
        for (int x = 0; x < tile_size && is_transparent; ++x) {
            int pixel_x = tile_x * tile_size + x;
            int pixel_y = tile_y * tile_size + y;

            Uint8* pixel_ptr = pixels + pixel_y * pitch + pixel_x * bpp;
            Uint32 pixel_value = *(Uint32*)pixel_ptr;

            Uint8 r, g, b, a;
            SDL_GetRGBA(pixel_value, surface->format, &r, &g, &b, &a);

            // Check if pixel is not transparent (alpha > 0)
            if (r == 255 and g == 0 and b == 255) {

            } else {
                if (a > 0) {
                    is_transparent = false;
                }
            }
        }
    }

    if (SDL_MUSTLOCK(surface)) {
        SDL_UnlockSurface(surface);
    }

    return is_transparent;
}



void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
    s16 xx = x;
    s16 yy = y;
    xx %= 512; // To emulate gba scroll wrapping.
    switch (layer) {
    case Layer::map_0_ext:
    case Layer::map_0:
        tile0_scroll = {(s32)xx, (s32)yy};
        break;

    case Layer::map_1_ext:
    case Layer::map_1:
        tile1_scroll = {(s32)xx, (s32)yy};
        break;

    case Layer::overlay:
        overlay_origin = {(Float)xx, (Float)yy};
        break;

    default:
        break;
    }
}



Vec2<u16> Platform::get_scroll(Layer layer)
{
    switch (layer) {
    case Layer::map_0_ext:
    case Layer::map_0:
        return {(u16)tile0_scroll.x, (u16)tile0_scroll.y};

    case Layer::map_1_ext:
    case Layer::map_1:
        return {(u16)tile1_scroll.x, (u16)tile1_scroll.y};

    default:
        return {};
    }
}



static SDL_Rect get_tile_source_rect_16x16(TileDesc tile_index,
                                           int texture_width)
{
    SDL_Rect src;

    const int tile_size = 16;
    const int tiles_per_row = texture_width / tile_size;

    int tile_x = tile_index % tiles_per_row;
    int tile_y = 0;

    src.x = tile_x * tile_size;
    src.y = tile_y * tile_size;
    src.w = tile_size;
    src.h = tile_size;

    return src;
}



static SDL_Rect get_tile_source_rect_8x8(TileDesc tile_index, int texture_width)
{
    SDL_Rect src;

    const int tile_size = 8;
    const int tiles_per_row = texture_width / tile_size;

    int tile_x = tile_index % tiles_per_row;
    int tile_y = tile_index / tiles_per_row;

    src.x = tile_x * tile_size;
    src.y = tile_y * tile_size;
    src.w = tile_size;
    src.h = tile_size;

    return src;
}



static std::string extract_texture_name(const char* path_or_name)
{
    std::string input(path_or_name);

    // Find the last path separator (/ or \)
    size_t last_slash = input.find_last_of("/\\");
    std::string filename = (last_slash != std::string::npos)
                               ? input.substr(last_slash + 1)
                               : input;

    // Remove any extension (.img.bin, .pal.bin, .png, etc.)
    size_t first_dot = filename.find('.');
    if (first_dot != std::string::npos) {
        filename = filename.substr(0, first_dot);
    }

    return filename;
}


SDL_Surface* expand_surface_to_width(SDL_Surface* loaded_surface, int width)
{
    if (loaded_surface->w < width) {
        const auto source_width = loaded_surface->w;
        const auto source_height = loaded_surface->h;
        auto expanded_surface =
            SDL_CreateRGBSurface(0,
                                 width,
                                 source_height,
                                 loaded_surface->format->BitsPerPixel,
                                 loaded_surface->format->Rmask,
                                 loaded_surface->format->Gmask,
                                 loaded_surface->format->Bmask,
                                 loaded_surface->format->Amask);

        if (!expanded_surface) {
            error("Failed to expand tile0 surface!");
        } else {
            Uint32 transparent_magenta =
                SDL_MapRGBA(expanded_surface->format, 0xFF, 0x00, 0xFF, 0x00);
            SDL_FillRect(expanded_surface, nullptr, transparent_magenta);
            SDL_Rect src_rect = {0, 0, source_width, source_height};
            SDL_Rect dst_rect = {0, 0, source_width, source_height};
            SDL_BlitSurface(
                loaded_surface, &src_rect, expanded_surface, &dst_rect);

            SDL_FreeSurface(loaded_surface);

            return expanded_surface;
        }
    }
    return loaded_surface;
}



void Platform::load_tile0_texture(const char* name_or_path)
{
    if (not renderer) {
        return;
    }

    auto name = extract_texture_name(name_or_path);

    if (tile0_texture) {
        SDL_DestroyTexture(tile0_texture);
        tile0_texture = nullptr;
    }
    if (tile0_surface) {
        SDL_FreeSurface(tile0_surface);
        tile0_surface = nullptr;
    }

    std::string full_path =
        resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* loaded_surface =
        load_png_with_stb(full_path, name.c_str(), ShaderPalette::tile0);
    if (!loaded_surface) {
        return;
    }

    tile0_index_zero_is_transparent =
        is_tile_transparent(loaded_surface, 0, 0, 0);

    // NOTE: don't do the expansion for externally loaded files, which are
    // neither flattened nor metatiled, as expanding them messes up texture
    // index mapping.
    if (std::string(name_or_path).find('.') == std::string::npos) {
        loaded_surface = expand_surface_to_width(loaded_surface, 2048);
    }

    // Keep the surface for later modification
    tile0_surface = loaded_surface;

    tile0_texture = SDL_CreateTextureFromSurface(renderer, tile0_surface);
    if (!tile0_texture) {
        error(format("Failed to create tile0 texture from %: %",
                     full_path.c_str(),
                     SDL_GetError()));
        SDL_FreeSurface(tile0_surface);
        tile0_surface = nullptr;
        return;
    }

    SDL_SetTextureBlendMode(tile0_texture, SDL_BLENDMODE_BLEND);

    tile0_texture_width = tile0_surface->w;
    tile0_texture_height = tile0_surface->h;
}



void Platform::load_tile1_texture(const char* name_or_path)
{
    if (not renderer) {
        return;
    }
    auto name = extract_texture_name(name_or_path);

    if (tile1_texture) {
        SDL_DestroyTexture(tile1_texture);
        tile1_texture = nullptr;
    }
    if (tile1_surface) {
        SDL_FreeSurface(tile1_surface);
        tile1_surface = nullptr;
    }

    std::string full_path =
        resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* loaded_surface =
        load_png_with_stb(full_path, name.c_str(), ShaderPalette::tile1);
    if (!loaded_surface) {
        return;
    }

    tile1_index_zero_is_transparent =
        is_tile_transparent(loaded_surface, 0, 0, 0);

    if (std::string(name_or_path).find('.') == std::string::npos) {
        loaded_surface = expand_surface_to_width(loaded_surface, 2048);
    }

    // Keep the surface for later modification
    tile1_surface = loaded_surface;

    tile1_texture = SDL_CreateTextureFromSurface(renderer, tile1_surface);
    if (!tile1_texture) {
        error(format("Failed to create tile1 texture from %: %",
                     full_path.c_str(),
                     SDL_GetError()));
        SDL_FreeSurface(tile1_surface);
        tile1_surface = nullptr;
        return;
    }

    SDL_SetTextureBlendMode(tile1_texture, SDL_BLENDMODE_BLEND);

    tile1_texture_width = tile1_surface->w;
    tile1_texture_height = tile1_surface->h;
}



TileDesc Platform::map_tile0_chunk(TileDesc src)
{
    if (src > 0) {
        return src + 127;
    }
    return 0;
}



TileDesc Platform::map_tile1_chunk(TileDesc src)
{
    if (src > 0) {
        return src + 127;
    }
    return 0;
}



static float wrap_y(float y)
{
    if (y > 160) {
        // FIXME: gba automatically wraps tile layers, and I happened to be
        // displaying a wrapped region of a tile layer on the gba. I could fix
        // it, but it's easier just to do this:
        y -= 508;
    }

    return y;
}



void Platform::set_overlay_origin(Float x, Float y)
{
    while (y < -256) {
        y += 256;
    }
    overlay_origin = {x, wrap_y(y)};
}



static SDL_Texture* current_sprite_texture = nullptr;
static SDL_Texture* sprite_mask_texture = nullptr;
static int sprite_texture_width = 0;
static int sprite_texture_height = 0;



void Platform::load_sprite_texture(const char* name)
{
    if (not renderer) {
        return;
    }

    // Clean up old texture if it exists
    if (current_sprite_texture) {
        SDL_DestroyTexture(current_sprite_texture);
        current_sprite_texture = nullptr;
    }
    if (sprite_mask_texture) {
        SDL_DestroyTexture(sprite_mask_texture);
        sprite_mask_texture = nullptr;
    }

    std::string full_path =
        resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* surface =
        load_png_with_stb(full_path, name, ShaderPalette::spritesheet);
    if (!surface) {
        error(format(
            "Failed to load image %: %", full_path.c_str(), IMG_GetError()));
        return;
    }

    // Convert indexed color to RGBA32 if needed
    if (surface->format->palette != nullptr) {
        SDL_Surface* converted =
            SDL_ConvertSurfaceFormat(surface, SDL_PIXELFORMAT_RGBA32, 0);
        SDL_FreeSurface(surface);
        if (!converted) {
            error(
                format("Failed to convert sprite surface: %", SDL_GetError()));
            return;
        }
        surface = converted;
    }

    // Create white mask version for color mixing
    SDL_Surface* mask_surf = SDL_CreateRGBSurfaceWithFormat(
        0, surface->w, surface->h, 32, SDL_PIXELFORMAT_RGBA32);
    if (mask_surf) {
        // Lock both surfaces
        if (SDL_MUSTLOCK(surface)) {
            SDL_LockSurface(surface);
        }
        SDL_LockSurface(mask_surf);

        Uint32* src_pixels = (Uint32*)surface->pixels;
        Uint32* dst_pixels = (Uint32*)mask_surf->pixels;
        int pixel_count = surface->w * surface->h;

        for (int i = 0; i < pixel_count; i++) {
            Uint8 r, g, b, a;
            SDL_GetRGBA(src_pixels[i], surface->format, &r, &g, &b, &a);
            if (r == 255 and g == 0 and b == 255) { // Non-transparent pixel
                dst_pixels[i] = 0;
            } else {
                dst_pixels[i] =
                    SDL_MapRGBA(mask_surf->format, 255, 255, 255, a);
            }
        }

        SDL_UnlockSurface(mask_surf);
        if (SDL_MUSTLOCK(surface)) {
            SDL_UnlockSurface(surface);
        }
    }

    SDL_Texture* mask_texture = nullptr;
    if (mask_surf) {
        mask_texture = SDL_CreateTextureFromSurface(renderer, mask_surf);
        SDL_FreeSurface(mask_surf);
    }

    // Set magenta (0xFF00FF) as the transparent color key
    Uint32 color_key = SDL_MapRGB(surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(surface, SDL_TRUE, color_key);

    current_sprite_texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!current_sprite_texture) {
        error(format("Failed to create texture from %: %",
                     full_path.c_str(),
                     SDL_GetError()));
        SDL_FreeSurface(surface);
        return;
    }

    sprite_mask_texture = mask_texture;

    // Enable alpha blending on the texture
    SDL_SetTextureBlendMode(current_sprite_texture, SDL_BLENDMODE_BLEND);

    sprite_texture_width = surface->w;
    sprite_texture_height = surface->h;

    SDL_FreeSurface(surface);
}



void Platform::clear_tile0_mappings()
{
}



void Platform::clear_tile1_mappings()
{
}



static Optional<Platform::UnrecoverrableErrorCallback>
    unrecoverrable_error_callback;



void Platform::fatal(const char* msg)
{
    error(msg);

    if (::__platform__ and ::unrecoverrable_error_callback) {
        (*::unrecoverrable_error_callback)(&*msg);
    }

    if (extensions.has_startup_opt("--regression")) {
        exit(EXIT_FAILURE);
    }

    PLATFORM.speaker().stop_music();

    const auto bkg_color = custom_color(0x007cbf);

    PLATFORM.screen().fade(1.f, bkg_color);
    PLATFORM.fill_overlay(0);
    PLATFORM.load_overlay_texture("overlay");
    PLATFORM.enable_glyph_mode(true);

    static constexpr const Text::OptColors text_colors{
        {custom_color(0xffffff), bkg_color}};

    static constexpr const Text::OptColors text_colors_inv{
        {text_colors->background_, text_colors->foreground_}};

    Text text({1, 1});
    text.append("fatal error:", text_colors_inv);

    Optional<Text> text2;

    Buffer<Text, 6> line_buffer;

    Optional<TextView> verbose_error;

    lisp::init(PLATFORM.load_file("", "/lisp_symtab.dat"),
               PLATFORM.load_file("", "/lisp_constant_tab.dat"));

    auto show_verbose_msg = [&] {
        PLATFORM.keyboard().poll();
        PLATFORM.screen().clear();

        text2.reset();
        line_buffer.clear();

        PLATFORM.screen().display();
        PLATFORM.screen().clear();

        verbose_error.emplace();
        verbose_error->assign(msg, {1, 3}, {28, 14}, 0, text_colors);

        PLATFORM.screen().display();
    };

    while (true) {
        show_verbose_msg();
        if (PLATFORM.keyboard().down_transition<Key::action_2>()) {
            PLATFORM.restart();
        }
    }
}



static void extract_text_colors_from_overlay()
{
    if (!overlay_surface) {
        warning("extract_text_colors_from_overlay: overlay_surface not "
                "initialized");
        return;
    }

    // Font color index tile is at index 81 (matches GBA)
    const int font_color_index_tile = 81;
    const int tile_size = 8;
    const int tiles_per_row = overlay_surface->w / tile_size;

    int tile_x = font_color_index_tile % tiles_per_row;
    int tile_y = font_color_index_tile / tiles_per_row;

    int pixel_x = tile_x * tile_size;
    int pixel_y = tile_y * tile_size;

    // Lock surface for pixel access
    if (SDL_MUSTLOCK(overlay_surface)) {
        if (SDL_LockSurface(overlay_surface) != 0) {
            error(format("Failed to lock surface: %", SDL_GetError()));
            return;
        }
    }

    // Get the first two pixels (indices 0 and 1) which contain fg and bg color info
    Uint8* pixels = (Uint8*)overlay_surface->pixels;
    int pitch = overlay_surface->pitch;

    // Get pixel at (pixel_x, pixel_y) - foreground color
    Uint8* fg_pixel = pixels + pixel_y * pitch +
                      pixel_x * overlay_surface->format->BytesPerPixel;
    Uint32 fg_pixel_value = *(Uint32*)fg_pixel;

    // Get pixel at (pixel_x + 1, pixel_y) - background color
    Uint8* bg_pixel = pixels + pixel_y * pitch +
                      (pixel_x + 1) * overlay_surface->format->BytesPerPixel;
    Uint32 bg_pixel_value = *(Uint32*)bg_pixel;

    // Convert to SDL_Color
    SDL_GetRGBA(fg_pixel_value,
                overlay_surface->format,
                &default_text_fg_color.r,
                &default_text_fg_color.g,
                &default_text_fg_color.b,
                &default_text_fg_color.a);

    SDL_GetRGBA(bg_pixel_value,
                overlay_surface->format,
                &default_text_bg_color.r,
                &default_text_bg_color.g,
                &default_text_bg_color.b,
                &default_text_bg_color.a);

    if (SDL_MUSTLOCK(overlay_surface)) {
        SDL_UnlockSurface(overlay_surface);
    }
}



std::string current_background_texture;



void Platform::load_background_texture(const char* name)
{
    if (not renderer) {
        return;
    }

    current_background_texture = name;
    auto texture_name = extract_texture_name(name);

    if (background_texture) {
        SDL_DestroyTexture(background_texture);
        background_texture = nullptr;
    }
    if (background_surface) {
        SDL_FreeSurface(background_surface);
        background_surface = nullptr;
    }

    std::string full_path =
        resource_path() + "images" + PATH_DELIMITER + texture_name + ".png";

    SDL_Surface* surface = load_png_with_stb(
        full_path, texture_name.c_str(), ShaderPalette::background);
    if (!surface) {
        return;
    }

    // Keep the surface for later modification
    background_surface = surface;

    background_texture =
        SDL_CreateTextureFromSurface(renderer, background_surface);
    if (!background_texture) {
        error(format("Failed to create background texture from %: %",
                     full_path.c_str(),
                     SDL_GetError()));
        SDL_FreeSurface(background_surface);
        background_surface = nullptr;
        return;
    }

    SDL_SetTextureBlendMode(background_texture, SDL_BLENDMODE_BLEND);

    background_texture_width = background_surface->w;
    background_texture_height = background_surface->h;
}



bool Platform::load_overlay_texture(const char* name)
{
    if (not renderer) {
        return true;
    }

    if (name == last_overlay_texture) {
        return true;
    }
    // Clean up old texture/surface if they exist
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
        current_overlay_texture = nullptr;
    }
    if (overlay_surface) {
        SDL_FreeSurface(overlay_surface);
        overlay_surface = nullptr;
    }

    // Clear glyph cache
    for (auto& entry : glyph_cache) {
        entry.in_use = false;
    }

    std::string full_path =
        resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* loaded_surface =
        load_png_with_stb(full_path, name, ShaderPalette::overlay);
    if (!loaded_surface) {
        error(format(
            "Failed to load overlay %: %", full_path.c_str(), IMG_GetError()));
        return false;
    }

    // CHECK DIMENSIONS BEFORE PROCEEDING

    if (loaded_surface->w == 0 || loaded_surface->h == 0) {
        error(format("Overlay % has invalid dimensions: %x%",
                     name,
                     loaded_surface->w,
                     loaded_surface->h));
        SDL_FreeSurface(loaded_surface);
        return false;
    }

    // Convert indexed color to RGBA32 if needed
    SDL_Surface* converted_surface = loaded_surface;
    if (loaded_surface->format->palette != nullptr) {
        converted_surface =
            SDL_ConvertSurfaceFormat(loaded_surface, SDL_PIXELFORMAT_RGBA32, 0);
        SDL_FreeSurface(loaded_surface);
        if (!converted_surface) {
            error(format("Failed to convert surface: %", SDL_GetError()));
            return false;
        }
        loaded_surface = converted_surface;
    }

    // NOW set color key on the RGBA surface
    Uint32 color_key = SDL_MapRGB(loaded_surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(loaded_surface, SDL_TRUE, color_key);

    // GBA limitation: match GBA VRAM layout with fixed width of 4032 pixels
    const int max_width = 4032;
    int source_width = std::min(loaded_surface->w, max_width);
    int source_height = loaded_surface->h;

    // Always create surface with max_width to match GBA VRAM expectations
    overlay_surface = SDL_CreateRGBSurface(0,
                                           max_width,
                                           source_height,
                                           loaded_surface->format->BitsPerPixel,
                                           loaded_surface->format->Rmask,
                                           loaded_surface->format->Gmask,
                                           loaded_surface->format->Bmask,
                                           loaded_surface->format->Amask);

    if (!overlay_surface) {
        error(format("Failed to create overlay surface: %", SDL_GetError()));
        SDL_FreeSurface(loaded_surface);
        return false;
    }

    // Fill entire surface with transparent magenta first
    Uint32 transparent_magenta =
        SDL_MapRGBA(overlay_surface->format, 0xFF, 0x00, 0xFF, 0x00);
    SDL_FillRect(overlay_surface, nullptr, transparent_magenta);

    // Copy the actual texture data into the beginning
    SDL_Rect src_rect = {0, 0, source_width, source_height};
    SDL_Rect dst_rect = {0, 0, source_width, source_height};
    SDL_BlitSurface(loaded_surface, &src_rect, overlay_surface, &dst_rect);

    SDL_FreeSurface(loaded_surface);

    // Extract text colors from tile 81 BEFORE creating the texture
    extract_text_colors_from_overlay();

    overlay_index_zero_is_transparent =
        is_tile_transparent(overlay_surface, 0, 0, 8);

    current_overlay_texture =
        SDL_CreateTextureFromSurface(renderer, overlay_surface);
    if (!current_overlay_texture) {
        error(format("Failed to create overlay texture from %: %",
                     full_path.c_str(),
                     SDL_GetError()));
        SDL_FreeSurface(overlay_surface);
        overlay_surface = nullptr;
        return false;
    }

    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);

    overlay_texture_width = max_width; // Always report max_width
    overlay_texture_height = source_height;

    // Calculate how many 8x8 tiles fit in the original texture
    overlay_base_tile_count =
        (overlay_texture_width / 8) * (overlay_texture_height / 8);

    last_overlay_texture = name;

    return true;
}



void Platform::on_unrecoverrable_error(UnrecoverrableErrorCallback callback)
{
    ::unrecoverrable_error_callback.emplace(callback);
}


Optional<std::chrono::microseconds> sleep_time;


void Platform::sleep(u32 frames)
{
    const auto amount =
        frames * (std::chrono::duration_cast<std::chrono::microseconds>(
                      std::chrono::seconds(1)) /
                  60);

    std::this_thread::sleep_for(amount);

    if (not sleep_time) {
        sleep_time = amount;
    } else {
        *sleep_time += amount;
    }
}



////////////////////////////////////////////////////////////////////////////////
// DeltaClock
////////////////////////////////////////////////////////////////////////////////



Platform::DeltaClock::DeltaClock()
{
}



std::chrono::time_point clk_prev = std::chrono::high_resolution_clock::now();
std::chrono::time_point clk_startup = std::chrono::high_resolution_clock::now();



Platform::DeltaClock::TimePoint Platform::DeltaClock::sample() const
{
    namespace chrono = std::chrono;
    auto clk = chrono::high_resolution_clock::now();
    return chrono::duration_cast<chrono::microseconds>(clk - clk_startup)
        .count();
}



static Microseconds last_delta = 1;



Microseconds Platform::DeltaClock::last_delta() const
{
    return ::last_delta;
}



Microseconds Platform::DeltaClock::reset()
{
    if (extensions.has_startup_opt("--validate-scripts") or
        extensions.has_startup_opt("--regression")) {
        return 16777;
    }

    namespace chrono = std::chrono;
    auto clk = chrono::high_resolution_clock::now();
    auto elapsed = chrono::duration_cast<chrono::microseconds>(clk - clk_prev);
    clk_prev = clk;
    auto slept = std::chrono::microseconds(0);
    if (sleep_time) {
        slept = *sleep_time;
        sleep_time.reset();
    }
    auto adjusted = elapsed - slept; // duration arithmetic works here
    ::last_delta = adjusted.count();
    return adjusted.count();
}


Platform::DeltaClock::~DeltaClock()
{
}



////////////////////////////////////////////////////////////////////////////////
// Shaders
////////////////////////////////////////////////////////////////////////////////



ColorConstant
passthrough_shader(ShaderPalette palette, ColorConstant k, int arg, int index)
{
    return k;
}



ColorConstant
contrast_shader(ShaderPalette palette, ColorConstant k, int arg, int index)
{
    return k;
}



ColorConstant grayscale_shader(int palette, ColorConstant k, int arg)
{
    return k; // TODO
}



ColorConstant contrast_shader(int palette, ColorConstant k, int arg)
{
    return k; // TODO
}



ColorConstant passthrough_shader(int palette, ColorConstant k, int arg)
{
    return k;
}



////////////////////////////////////////////////////////////////////////////////
// NetworkPeer
////////////////////////////////////////////////////////////////////////////////



Platform::NetworkPeer::NetworkPeer() : impl_(nullptr)
{
}


void Platform::NetworkPeer::disconnect()
{
}


bool Platform::NetworkPeer::is_host() const
{
    return true;
}


void Platform::NetworkPeer::listen()
{
    return;
}


void Platform::NetworkPeer::connect(const char* peer)
{
    return;
}


bool Platform::NetworkPeer::is_connected() const
{
    return false; // TODO
}


bool Platform::NetworkPeer::send_message(const Message& message)
{
    return true; // TODO
}


static std::vector<u8> receive_buffer;


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


Platform::NetworkPeer::~NetworkPeer()
{
}


Platform::NetworkPeer::Interface Platform::NetworkPeer::interface() const
{
    return Interface::internet;
}


int Platform::NetworkPeer::send_queue_size() const
{
    return 100000;
}



////////////////////////////////////////////////////////////////////////////////
// RemoteConsole
////////////////////////////////////////////////////////////////////////////////



#include <mutex>
#include <queue>
#include <string>
#include <thread>
#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
typedef SOCKET socket_t;
#define CLOSE_SOCKET closesocket
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
typedef int socket_t;
#define CLOSE_SOCKET close
#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#endif

namespace
{
std::thread server_thread;
std::thread client_thread;
socket_t server_socket = INVALID_SOCKET;
socket_t client_socket = INVALID_SOCKET;
std::mutex console_mutex;
std::queue<std::string> incoming_lines;
std::queue<std::string> outgoing_lines;
std::atomic<bool> server_running = false;
} // namespace

static void handle_client(socket_t sock)
{
    char buffer[1024];
    std::string line_buffer;

    // Set socket timeout for recv
#ifdef _WIN32
    DWORD timeout = 500; // 500ms
    setsockopt(
        sock, SOL_SOCKET, SO_RCVTIMEO, (const char*)&timeout, sizeof(timeout));
    // Enable TCP keepalive
    BOOL keepalive = TRUE;
    setsockopt(sock,
               SOL_SOCKET,
               SO_KEEPALIVE,
               (const char*)&keepalive,
               sizeof(keepalive));
#else
    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 500000; // 500ms
    setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    // Enable TCP keepalive
    int keepalive = 1;
    setsockopt(sock, SOL_SOCKET, SO_KEEPALIVE, &keepalive, sizeof(keepalive));
#endif

    const char* welcome = "Skyland Remote Console\n> ";
    if (send(sock, welcome, strlen(welcome), 0) <= 0) {
        CLOSE_SOCKET(sock);
        client_socket = INVALID_SOCKET;
        return;
    }

    while (server_running) {
        // Check for output to send
        bool has_output = false;
        {
            std::lock_guard<std::mutex> lock(console_mutex);
            has_output = !outgoing_lines.empty();
        }

        if (has_output) {
            std::lock_guard<std::mutex> lock(console_mutex);
            while (!outgoing_lines.empty()) {
                std::string msg = outgoing_lines.front() + "\n> ";
                outgoing_lines.pop();
                // Check if send fails (client disconnected)
                if (send(sock, msg.c_str(), msg.length(), 0) <= 0) {
                    printf("Client disconnected (send failed)\n");
                    CLOSE_SOCKET(sock);
                    client_socket = INVALID_SOCKET;
                    return;
                }
            }
        }

        // Try to receive data
        int received = recv(sock, buffer, sizeof(buffer) - 1, 0);

        if (received > 0) {
            buffer[received] = '\0';
            for (int i = 0; i < received; i++) {
                if (buffer[i] == '\n' || buffer[i] == '\r') {
                    if (!line_buffer.empty()) {
                        std::lock_guard<std::mutex> lock(console_mutex);
                        incoming_lines.push(line_buffer);
                        line_buffer.clear();
                    }
                } else {
                    line_buffer += buffer[i];
                }
            }
        } else if (received == 0) {
            // Graceful disconnect
            printf("Client disconnected gracefully\n");
            break;
        } else {
            // Check for real errors vs timeout
#ifdef _WIN32
            int err = WSAGetLastError();
            if (err != WSAETIMEDOUT && err != WSAEWOULDBLOCK) {
                printf("Client disconnected (recv error: %d)\n", err);
                break;
            }
#else
            if (errno != EAGAIN && errno != EWOULDBLOCK) {
                printf("Client disconnected (recv error: %d)\n", errno);
                break;
            }
#endif
        }
    }

    CLOSE_SOCKET(sock);
    client_socket = INVALID_SOCKET;
}

void server_thread_main()
{
#ifdef _WIN32
    WSADATA wsaData;
    WSAStartup(MAKEWORD(2, 2), &wsaData);
#endif

    server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (server_socket == INVALID_SOCKET) {
        return;
    }

    // Allow address reuse
    int opt = 1;
    setsockopt(server_socket,
               SOL_SOCKET,
               SO_REUSEADDR,
               (const char*)&opt,
               sizeof(opt));

    Conf conf;

    sockaddr_in addr = {};
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = inet_addr("127.0.0.1"); // localhost only
    addr.sin_port =
        htons(conf.expect<Conf::Integer>("hardware.desktop", "console_port"));

    if (bind(server_socket, (sockaddr*)&addr, sizeof(addr)) == SOCKET_ERROR) {
        CLOSE_SOCKET(server_socket);
        return;
    }

    if (listen(server_socket, 1) == SOCKET_ERROR) {
        CLOSE_SOCKET(server_socket);
        return;
    }

    // Set accept timeout
#ifdef _WIN32
    DWORD timeout = 1000; // 1 second
    setsockopt(server_socket,
               SOL_SOCKET,
               SO_RCVTIMEO,
               (const char*)&timeout,
               sizeof(timeout));
#else
    struct timeval timeout;
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;
    setsockopt(
        server_socket, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
#endif

    while (server_running) {
        socket_t new_client = accept(server_socket, nullptr, nullptr);

        if (new_client != INVALID_SOCKET) {
            // Close existing client if any
            if (client_socket != INVALID_SOCKET) {
                CLOSE_SOCKET(client_socket);
                if (client_thread.joinable()) {
                    client_thread.join();
                }
            }

            client_socket = new_client;
            client_thread = std::thread(handle_client, new_client);
        }
        // If accept fails/times out, it just loops again
    }

    if (client_socket != INVALID_SOCKET) {
        CLOSE_SOCKET(client_socket);
    }
    if (client_thread.joinable()) {
        client_thread.join();
    }

    CLOSE_SOCKET(server_socket);

#ifdef _WIN32
    WSACleanup();
#endif
}

void Platform::RemoteConsole::start()
{
    if (server_running) {
        return;
    }
#ifndef __EMSCRIPTEN__
    server_running = true;
    server_thread = std::thread(server_thread_main);
#endif
}


auto Platform::RemoteConsole::readline() -> Optional<Line>
{
    std::lock_guard<std::mutex> lock(console_mutex);
    if (!incoming_lines.empty()) {
        Line ret = incoming_lines.front().c_str();
        incoming_lines.pop();
        return ret;
    }
    return {};
}

bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    std::lock_guard<std::mutex> lock(console_mutex);
    outgoing_lines.push(std::string(text));
    return client_socket != INVALID_SOCKET;
}


////////////////////////////////////////////////////////////////////////////////
// Logger
////////////////////////////////////////////////////////////////////////////////


static const char* const logfile_name = "logfile.txt";
static std::ofstream logfile_out(logfile_name);


static Severity log_threshold;


Optional<Vector<char>> log_data_;


void Platform::Logger::clear()
{
    log_data_.reset();
}


void Platform::Logger::set_threshold(Severity severity)
{
    log_threshold = severity;
}


void Platform::Logger::flush()
{
    if (not log_data_) {
        return;
    }

    flash_filesystem::store_file_data_binary("/log.txt", *log_data_);

    log_data_.reset();
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

    while (*msg not_eq '\0') {
        log_data_->push_back(*msg, t);
        std::cout << *msg;
        ++msg;
    }

    log_data_->push_back('\n', t);
    std::cout << '\n';
}



Platform::Logger::Logger()
{
}



Vector<char>* Platform::Logger::data()
{
    if (log_data_) {
        return &*log_data_;
    }

    return nullptr;
}



////////////////////////////////////////////////////////////////////////////////
// Screen
////////////////////////////////////////////////////////////////////////////////



Platform::Screen::Screen() : userdata_(nullptr)
{
}



void Platform::Screen::set_shader(Shader shader)
{
    current_shader = shader;
}



void Platform::Screen::set_shader_argument(int arg)
{
    current_shader_arg = arg;
    if (not current_background_texture.empty()) {
        PLATFORM.load_background_texture(current_background_texture.c_str());
    }
}



struct SpriteDrawInfo
{
    Vec2<s32> position;
    Sprite::Size size;
    ColorConstant color;
    bool visible;
    u16 texture_index;
    Vec2<bool> flip;
    double rotation;
    u8 priority;
    Sprite::Alpha alpha;
    u8 mix_amount;
    Vec2<double> scale;
};



static std::vector<SpriteDrawInfo> sprite_draw_list;



void Platform::Screen::draw_batch(TextureIndex t,
                                  const Buffer<Vec2<s32>, 64>& coords,
                                  const SpriteBatchOptions& opts)
{
    // There's no need for an optimized batch drawing function like what we
    // needed on gba.
    auto view_center = get_view().int_center().cast<s32>();

    for (const auto& coord : coords) {
        Vec2<s32> pos = coord;

        if (opts.position_absolute_) {
            pos = pos + view_center;
        }

        Sprite spr;
        spr.set_size(opts.sz_);
        spr.set_position(Vec2<Fixnum>{Fixnum::from_integer(pos.x),
                                      Fixnum::from_integer(pos.y)});
        spr.set_alpha(opts.alpha_);
        spr.set_texture_index(t);
        draw(spr);
    }
}



static SDL_Rect get_sprite_source_rect(u16 texture_index, Sprite::Size size)
{
    SDL_Rect src;

    // Check if this texture index is in the dynamic range and has been remapped
    if (is_dynamic_texture_index(texture_index, size)) {
        u8 slot = get_dynamic_slot_for_index(texture_index, size);
        auto& mapping = dynamic_texture_mappings[slot];

        if (mapping.reserved_) {
            auto input_idx = texture_index;
            // Use the remapped spritesheet offset instead of the texture_index
            texture_index = mapping.spritesheet_offset_;
            if (size == Sprite::Size::w16_h16) {
                texture_index *= 2;
                texture_index += input_idx % 2;
            }
            if (size == Sprite::Size::w32_h32) {
                texture_index /= 2;
            }
        }
        // If not reserved, just use the original texture_index (shows default position)
    }

    // Normal sprite calculation using the (possibly remapped) texture_index
    const int meta_tile_width = 16;
    const int meta_tile_height = 32;
    int tiles_per_row = sprite_texture_width / meta_tile_width;
    int meta_tile_x = texture_index % tiles_per_row;
    int meta_tile_y = texture_index / tiles_per_row;
    int base_x = meta_tile_x * meta_tile_width;
    int base_y = meta_tile_y * meta_tile_height;

    switch (size) {
    case Sprite::Size::w8_h8: {
        int base_x = (texture_index / 8) * 16;
        base_x += 8 * (texture_index % 2);
        int base_y = ((texture_index % 8) / 2) * 8;
        src.x = base_x;
        src.y = base_y;
        src.w = 8;
        src.h = 8;
        break;
    }
    case Sprite::Size::w16_h16: {
        int base_x = (texture_index / 2) * 16;
        src.x = base_x;
        src.y = 16 * (texture_index % 2);
        src.w = 16;
        src.h = 16;
        break;
    }
    case Sprite::Size::w16_h32:
        src.x = base_x;
        src.y = base_y;
        src.w = 16;
        src.h = 32;
        break;
    case Sprite::Size::w32_h32:
        src.x = base_x * 2;
        src.y = base_y;
        src.w = 32;
        src.h = 32;
        break;
    }

    return src;
}



static double sprite_rotation_to_degrees(s16 rotation)
{
    // The GBA code uses -(INT16_MAX / 360) * degrees
    // So rotation ranges from 0 to 32767 for a full 360Ã‚Â° rotation
    // We need to convert back to degrees
    return (rotation * 360.0) / 32767.0;
}



void Platform::Screen::draw(const Sprite& spr)
{
    if (spr.get_alpha() == Sprite::Alpha::transparent) {
        return;
    }

    auto pos = ivec(spr.get_position()) - spr.get_origin().cast<s32>();

    auto view_center = get_view().int_center().cast<s32>();
    pos = pos - view_center;
    pos.y = wrap_y(pos.y);

    // Convert GBA scale to SDL scale
    // GBA: pa() = (1 << 8) - sx
    // The actual scale is 256 / pa()
    double scale_x = 1.0;
    double scale_y = 1.0;

    if (spr.get_scale().x != 0 || spr.get_scale().y != 0) {
        double pa = 256.0 - spr.get_scale().x;
        double pd = 256.0 - spr.get_scale().y;
        scale_x = 256.0 / pa;
        scale_y = 256.0 / pd;
    }

    sprite_draw_list.push_back({pos,
                                spr.get_size(),
                                spr.get_mix().color_,
                                true,
                                spr.get_texture_index(),
                                spr.get_flip(),
                                sprite_rotation_to_degrees(-spr.get_rotation()),
                                spr.get_priority(),
                                spr.get_alpha(),
                                spr.get_mix().amount_,
                                {scale_x, scale_y}});
}



void Platform::Screen::clear()
{
    if (extensions.has_startup_opt("--no-window-system")) {
        // In windowless mode, without vsync, the game will needlessly burn cpu utilization.
        std::this_thread::sleep_for(std::chrono::milliseconds(17));
    }

    if (not renderer) {
        return;
    }

    rect_draw_queue.clear();
    sprite_draw_list.clear();
    point_lights.clear();

    if (extensions.has_startup_opt("--validate-scripts") or
        extensions.has_startup_opt("--regression")) {
        return;
    }

    // Clear entire screen to black first (for letterboxing)
    SDL_SetRenderDrawColor(renderer, 0, 0, 16, 255);
    SDL_RenderClear(renderer);

    auto bkg_color =
        current_shader(ShaderPalette::background, custom_color(0x5fa8ea), 0, 4);

    auto clr = color_to_sdl(bkg_color);
    SDL_SetRenderDrawColor(renderer, clr.r, clr.g, clr.b, clr.a);
    SDL_Rect game_area = {0, 0, 240, 160};
    SDL_RenderFillRect(renderer, &game_area);
}



static SDL_Rect get_overlay_tile_source_rect(TileDesc tile_index)
{
    SDL_Rect src;

    if (!current_overlay_texture) {
        src.x = 0;
        src.y = 0;
        src.w = 8;
        src.h = 8;
        return src;
    }

    // Overlay tiles are 8x8 pixels
    const int tile_size = 8;
    const int tiles_per_row = overlay_texture_width / tile_size;

    int tile_x = tile_index % tiles_per_row;
    int tile_y = tile_index / tiles_per_row;

    src.x = tile_x * tile_size;
    src.y = tile_y * tile_size;
    src.w = tile_size;
    src.h = tile_size;

    return src;
}



static float last_fade_amt;
static ColorConstant fade_color;
static bool fade_include_overlay;


void Platform::Screen::fade(float amount,
                            ColorConstant k,
                            Optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{
    last_fade_amt = amount;
    fade_color = k;
    fade_include_overlay = include_overlay;
}



bool Platform::Screen::fade_active() const
{
    return last_fade_amt > 0;
}



void Platform::Screen::schedule_fade(Float amount, const FadeProperties& props)
{
    last_fade_amt = amount;
    fade_color = props.color;
    fade_include_overlay = props.include_overlay;
}



void display_fade()
{
    if (not renderer) {
        return;
    }

    if (last_fade_amt == 0) {
        return;
    }

    SDL_Rect rect;
    rect.x = 0;
    rect.y = 0;
    rect.w = 240;
    rect.h = 160;

    auto color = color_to_sdl(fade_color);
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(
        renderer, color.r, color.g, color.b, 255 * last_fade_amt);
    SDL_RenderFillRect(renderer, &rect);
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
}



void draw_rect_group(int prio)
{
    if (not renderer) {
        return;
    }

    for (auto& rect_info : reversed(rect_draw_queue)) {
        if (rect_info.priority not_eq prio) {
            continue;
        }

        SDL_Rect rect;
        rect.x = rect_info.x;
        rect.y = rect_info.y;
        rect.w = rect_info.w;
        rect.h = rect_info.h;

        auto color = color_to_sdl(rect_info.tint);
        SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a);
        SDL_RenderFillRect(renderer, &rect);
    }
}



void draw_sprite_group(int prio)
{
    if (not renderer) {
        return;
    }

    // Draw all sprites as colored rectangles or textured
    for (const auto& sprite : reversed(sprite_draw_list)) {
        if (sprite.priority not_eq prio) {
            continue;
        }
        if (!sprite.visible)
            continue;

        if (!current_sprite_texture) {
            // Fallback to colored rectangles if no texture loaded
            SDL_Rect rect;
            rect.x = sprite.position.x;
            rect.y = sprite.position.y;

            // Set size based on sprite size enum
            switch (sprite.size) {
            case Sprite::Size::w8_h8:
                rect.w = 8;
                rect.h = 8;
                break;
            case Sprite::Size::w16_h16:
                rect.w = 16;
                rect.h = 16;
                break;
            case Sprite::Size::w16_h32:
                rect.w = 16;
                rect.h = 32;
                break;
            case Sprite::Size::w32_h32:
                rect.w = 32;
                rect.h = 32;
                break;
            }

            // Set color and draw
            auto color = color_to_sdl(sprite.color);
            SDL_SetRenderDrawColor(
                renderer, color.r, color.g, color.b, color.a);
            SDL_RenderFillRect(renderer, &rect);

            // Draw border for visibility
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
            SDL_RenderDrawRect(renderer, &rect);
        } else {
            SDL_Rect src =
                get_sprite_source_rect(sprite.texture_index, sprite.size);

            SDL_Rect dst;
            dst.w = src.w * sprite.scale.x; // Apply scale
            dst.h = src.h * sprite.scale.y; // Apply scale
            dst.x = sprite.position.x - (dst.w - src.w) / 2;
            dst.y = sprite.position.y - (dst.h - src.h) / 2;

            // Handle flipping
            SDL_RendererFlip flip = SDL_FLIP_NONE;
            if (sprite.flip.x && sprite.flip.y) {
                flip =
                    (SDL_RendererFlip)(SDL_FLIP_HORIZONTAL | SDL_FLIP_VERTICAL);
            } else if (sprite.flip.x) {
                flip = SDL_FLIP_HORIZONTAL;
            } else if (sprite.flip.y) {
                flip = SDL_FLIP_VERTICAL;
            }

            // Calculate base alpha
            u8 base_alpha =
                (sprite.alpha == Sprite::Alpha::translucent) ? 127 : 255;

            if (sprite.color != ColorConstant::null && sprite.mix_amount > 0 and
                sprite_mask_texture) {
                // Dual-pass rendering with scaling
                float mix_ratio = sprite.mix_amount / 255.0f;

                SDL_SetTextureColorMod(current_sprite_texture, 255, 255, 255);
                SDL_SetTextureAlphaMod(current_sprite_texture,
                                       base_alpha * (1.0f - mix_ratio));
                SDL_RenderCopyEx(renderer,
                                 current_sprite_texture,
                                 &src,
                                 &dst,
                                 sprite.rotation,
                                 nullptr,
                                 flip);

                auto mix_color = color_to_sdl(sprite.color);
                SDL_SetTextureColorMod(
                    sprite_mask_texture, mix_color.r, mix_color.g, mix_color.b);
                SDL_SetTextureAlphaMod(sprite_mask_texture,
                                       base_alpha * mix_ratio);
                SDL_RenderCopyEx(renderer,
                                 sprite_mask_texture,
                                 &src,
                                 &dst,
                                 sprite.rotation,
                                 nullptr,
                                 flip);

                SDL_SetTextureColorMod(sprite_mask_texture, 255, 255, 255);
                SDL_SetTextureAlphaMod(sprite_mask_texture, 255);
                SDL_SetTextureColorMod(current_sprite_texture, 255, 255, 255);
                SDL_SetTextureAlphaMod(current_sprite_texture, 255);
            } else {
                SDL_SetTextureColorMod(current_sprite_texture, 255, 255, 255);
                SDL_SetTextureAlphaMod(current_sprite_texture, base_alpha);

                SDL_RenderCopyEx(renderer,
                                 current_sprite_texture,
                                 &src,
                                 &dst,
                                 sprite.rotation,
                                 nullptr,
                                 flip);

                SDL_SetTextureAlphaMod(current_sprite_texture, 255);
            }
        }
    }
}



static void draw_parallax_background(
    SDL_Texture* texture,
    int texture_width,
    const View& view,
    const ParallaxStrip& strip1, // gradient (rows 0-13)
    const ParallaxStrip& strip2, // dark clouds (rows 14-15)
    const ParallaxStrip& strip3) // white clouds (rows 16-19)
{
    if (not renderer) {
        return;
    }

    if (!texture) {
        return;
    }

    auto& tiles = tile_layers_[Layer::background];
    if (tiles.empty()) {
        return;
    }

    auto draw_strip = [&](const ParallaxStrip& strip) {
        const int tile_size = 8;
        const int wrap_width = 256; // 32 tiles * 8 pixels

        for (auto& [pos, tile_info] : tiles) {
            if (tile_info.tile_desc == 0)
                continue;

            s32 tile_x = (s32)pos.first;
            s32 tile_y = (s32)pos.second;

            // Skip tiles not in this strip's row range
            if (tile_y < strip.start_tile_row || tile_y > strip.end_tile_row) {
                continue;
            }

            SDL_Rect src =
                get_tile_source_rect_8x8(tile_info.tile_desc, texture_width);

            // Calculate base position with scroll
            s32 base_x = tile_x * tile_size - strip.scroll.x;
            s32 base_y = tile_y * tile_size - strip.scroll.y;

            SDL_Rect dst;
            dst.x = base_x;
            dst.y = base_y;
            dst.w = tile_size;
            dst.h = tile_size;

            // Draw at base position if on screen
            if (dst.x + dst.w > 0 && dst.x < 240 && dst.y + dst.h > 0 &&
                dst.y < 160) {
                SDL_RenderCopy(renderer, texture, &src, &dst);
            }

            // Draw wrapped copy if needed (either +256 or -256)
            if (dst.x < 0) {
                dst.x += wrap_width;
                if (dst.x < 240) {
                    SDL_RenderCopy(renderer, texture, &src, &dst);
                }
            } else if (dst.x >= 240) {
                dst.x -= wrap_width;
                if (dst.x + dst.w > 0) {
                    SDL_RenderCopy(renderer, texture, &src, &dst);
                }
            }
        }
    };

    // Gap filler strip (rows 20-21) - positioned flush with end of strip2
    // This fills the gap between cloud layers during vertical parallax
    ParallaxStrip gap_filler;
    gap_filler.start_tile_row = 20;
    gap_filler.end_tile_row = 21;
    gap_filler.scroll = strip2.scroll;
    gap_filler.scroll.y = strip2.scroll.y + 32; // 32 pixels = 4 tile rows

    // Bottom filler strip (rows 18-19 repeated) - positioned beneath strip3
    // This fills the gap at the bottom when strip3 scrolls up
    ParallaxStrip bottom_filler;
    bottom_filler.start_tile_row = 18;
    bottom_filler.end_tile_row = 19;
    bottom_filler.scroll = strip3.scroll;
    bottom_filler.scroll.y =
        strip3.scroll.y - 16; // Bump down 32 pixels (4 rows)

    // Draw all strips
    draw_strip(strip1);
    draw_strip(strip2);
    draw_strip(gap_filler);
    draw_strip(strip3);
    draw_strip(bottom_filler);
}



static void draw_tile_layer(Layer layer,
                            SDL_Texture* texture,
                            int texture_width,
                            const Vec2<s32>& scroll,
                            const View& view)
{
    if (not renderer) {
        return;
    }

    if (!texture) {
        return;
    }

    auto& tiles = tile_layers_[layer];
    if (tiles.empty()) {
        return;
    }

    bool skip_tile_zero = true;
    if (layer == Layer::map_0_ext || layer == Layer::map_0) {
        skip_tile_zero = tile0_index_zero_is_transparent;
    } else if (layer == Layer::map_1_ext || layer == Layer::map_1) {
        skip_tile_zero = tile1_index_zero_is_transparent;
    } else if (layer == Layer::overlay) {
        skip_tile_zero = overlay_index_zero_is_transparent;
    }

    // Check if this layer should be drawn with translucence
    bool apply_translucence = false;
    if (layer == Layer::map_0_ext || layer == Layer::map_0) {
        apply_translucence = tile0_translucent;
    } else if (layer == Layer::map_1_ext || layer == Layer::map_1) {
        apply_translucence = tile1_translucent;
    }

    // Set alpha for translucent layers
    if (apply_translucence) {
        SDL_SetTextureAlphaMod(texture, 127);
    }

    auto view_center = view.int_center().cast<s32>();
    bool is_ext_layer =
        (layer == Layer::map_0_ext || layer == Layer::map_1_ext);

    for (auto& [pos, tile_info] : tiles) {
        if (skip_tile_zero and tile_info.tile_desc == 0)
            continue;

        s32 tile_x = (s32)pos.first;
        s32 tile_y = (s32)pos.second;

        SDL_Rect src;
        SDL_Rect dst;

        if (is_ext_layer) {
            src =
                get_tile_source_rect_16x16(tile_info.tile_desc, texture_width);
            dst.x = tile_x * 16 - scroll.x - view_center.x;
            dst.y = wrap_y(tile_y * 16 - scroll.y - view_center.y);
            dst.w = 16;
            dst.h = 16;
        } else {
            src = get_tile_source_rect_8x8(tile_info.tile_desc, texture_width);
            dst.x = tile_x * 8 - scroll.x - view_center.x;
            dst.y = wrap_y(tile_y * 8 - scroll.y - view_center.y);
            dst.w = 8;
            dst.h = 8;
        }

        // Check if this tile uses a special palette (13, 14, or 15)
        auto special_it = special_palettes.find(tile_info.palette);
        if (special_it != special_palettes.end()) {
            // Render to intermediate buffer
            SDL_SetRenderTarget(renderer, tile_recolor_buffer);

            // Fill buffer with the solid color
            SDL_SetRenderDrawColor(renderer,
                                   special_it->second.r,
                                   special_it->second.g,
                                   special_it->second.b,
                                   0); // Alpha 0 for now
            SDL_RenderClear(renderer);

            // Copy ONLY the alpha channel from the tile
            // Draw the tile but use a custom blend mode that only copies alpha
            SDL_Rect temp_dst = {0, 0, dst.w, dst.h};

            // Set color mod to black so RGB doesn't affect anything
            SDL_SetTextureColorMod(texture, 0, 0, 0);
            SDL_SetTextureAlphaMod(texture, 255);

            // Use a blend mode that copies the source alpha to destination alpha
            SDL_SetTextureBlendMode(
                texture,
                SDL_ComposeCustomBlendMode(
                    SDL_BLENDFACTOR_ZERO, // srcColorFactor (ignore source RGB)
                    SDL_BLENDFACTOR_ONE, // dstColorFactor (keep our solid color)
                    SDL_BLENDOPERATION_ADD, // colorOperation
                    SDL_BLENDFACTOR_ONE,    // srcAlphaFactor (use source alpha)
                    SDL_BLENDFACTOR_ZERO, // dstAlphaFactor (replace dest alpha)
                    SDL_BLENDOPERATION_ADD // alphaOperation
                    ));

            SDL_RenderCopy(renderer, texture, &src, &temp_dst);

            // Reset texture settings
            SDL_SetTextureColorMod(texture, 255, 255, 255);
            SDL_SetTextureAlphaMod(texture, apply_translucence ? 127 : 255);
            SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);

            // Switch back to main screen
            SDL_SetRenderTarget(renderer, nullptr);

            // Draw the recolored tile to screen
            SDL_RenderCopy(renderer, tile_recolor_buffer, &temp_dst, &dst);
        } else {
            if (tile_info.palette == 9) {
                SDL_SetTextureColorMod(texture, 127, 127, 127);
                SDL_RenderCopy(renderer, texture, &src, &dst);
                SDL_SetTextureColorMod(texture, 255, 255, 255);
            } else {
                SDL_RenderCopy(renderer, texture, &src, &dst);
            }
        }
    }

    // Reset alpha after drawing
    if (apply_translucence) {
        SDL_SetTextureAlphaMod(texture, 255);
    }
}



ColorConstant bg_color_default()
{
    // ...
    return ColorConstant::rich_black;
}



void SDL_RenderFillCircle(SDL_Renderer* renderer,
                          int cx,
                          int cy,
                          int radius,
                          Uint8 r,
                          Uint8 g,
                          Uint8 b,
                          Uint8 a)
{
    SDL_SetRenderDrawColor(renderer, r, g, b, a);
    for (int y = -radius; y <= radius; y++) {
        int width = (int)sqrt(radius * radius - y * y);
        SDL_RenderDrawLine(renderer, cx - width, cy + y, cx + width, cy + y);
    }
}



void draw_overlay_layer(int y_offset)
{
    if (not renderer) {
        return;
    }

    auto& overlay_tiles = tile_layers_[Layer::overlay];
    for (auto& [pos, tile_info] : overlay_tiles) {
        auto tile_desc = tile_info.tile_desc;
        if (tile_desc == 0)
            continue;

        SDL_Rect src = get_overlay_tile_source_rect(tile_desc);
        SDL_Rect dst;
        dst.x = pos.first * 8 - overlay_origin.x;
        dst.y = pos.second * 8 - (overlay_origin.y + y_offset);
        dst.w = 8;
        dst.h = 8;

        // Draw background color for glyphs
        if (is_glyph(tile_desc)) {
            ColorConstant bg_color_key;
            bool skip = false;
            if ((int)tile_info.text_bg_color_ != 0) {
                bg_color_key = tile_info.text_bg_color_;
            } else {
                bg_color_key = (ColorConstant)((default_text_bg_color.r << 16) |
                                               (default_text_bg_color.g << 8) |
                                               default_text_bg_color.b);
                skip = default_text_bg_color.a == 0;
            }

            if (not skip) {
                auto color = color_to_sdl(bg_color_key);
                SDL_SetRenderDrawColor(
                    renderer, color.r, color.g, color.b, 255);
                SDL_RenderFillRect(renderer, &dst);
            }
        }

        // Apply foreground color if specified, otherwise use default
        if ((int)tile_info.text_fg_color_ != 0) {
            auto fg_color = color_to_sdl(tile_info.text_fg_color_);
            SDL_SetTextureColorMod(
                current_overlay_texture, fg_color.r, fg_color.g, fg_color.b);
        } else if (is_glyph(tile_desc)) {
            // Use extracted default foreground color for glyphs
            SDL_SetTextureColorMod(current_overlay_texture,
                                   default_text_fg_color.r,
                                   default_text_fg_color.g,
                                   default_text_fg_color.b);
        }

        SDL_RenderCopy(renderer, current_overlay_texture, &src, &dst);

        // Reset color mod
        SDL_SetTextureColorMod(current_overlay_texture, 255, 255, 255);
    }
}



void Platform::Screen::display()
{
    if (not renderer) {
        return;
    }

    if (extensions.has_startup_opt("--validate-scripts") or
        extensions.has_startup_opt("--regression")) {
        return;
    }

    if (background_texture) {
        if (parallax_clouds_enabled) {
            draw_parallax_background(background_texture,
                                     background_texture_width,
                                     get_view(),
                                     parallax_strip1,
                                     parallax_strip2,
                                     parallax_strip3);
        } else {
            // Normal uniform scrolling
            draw_tile_layer(Layer::background,
                            background_texture,
                            background_texture_width,
                            {0, 0},
                            get_view());
        }
    }

    draw_rect_group(3);
    draw_sprite_group(3);
    draw_tile_layer(Layer::map_1_ext,
                    tile1_texture,
                    tile1_texture_width,
                    tile1_scroll,
                    get_view());
    draw_tile_layer(Layer::map_1,
                    tile1_texture,
                    tile1_texture_width,
                    tile1_scroll,
                    get_view());
    // Draw tile0 layer
    draw_tile_layer(Layer::map_0_ext,
                    tile0_texture,
                    tile0_texture_width,
                    tile0_scroll,
                    get_view());
    draw_tile_layer(Layer::map_0,
                    tile0_texture,
                    tile0_texture_width,
                    tile0_scroll,
                    get_view());
    if (tile0_scroll.x > 240 and tile0_scroll.x < 512) {
        // Hack to account for gba scroll wrapping, in the inteval between where
        // the x coordinate of a scrolled layer wraps.
        auto scroll_tmp = tile0_scroll;
        scroll_tmp.x -= 512;
        draw_tile_layer(Layer::map_0,
                        tile0_texture,
                        tile0_texture_width,
                        scroll_tmp,
                        get_view());
    }
    draw_rect_group(2);
    draw_sprite_group(2);
    draw_rect_group(1);
    draw_sprite_group(1);

    if (not fade_include_overlay) {
        display_fade();
    }

    for (auto [x, y, radius, tint, intensity] : point_lights) {
        SDL_Texture* light_texture = create_point_light_texture(radius);
        if (!light_texture) {
            return;
        }
        // Apply color tint
        auto color = color_to_sdl(tint);
        SDL_SetTextureColorMod(light_texture, color.r, color.g, color.b);
        SDL_SetTextureAlphaMod(light_texture, intensity);

        // Transform to screen coordinates (relative to view)
        auto view_center =
            PLATFORM.screen().get_view().int_center().cast<s32>();
        s32 screen_x = x.as_integer() - view_center.x;
        s32 screen_y = wrap_y(y.as_integer() - view_center.y);

        // Position the light (centered on transformed coordinates)
        SDL_Rect dst;
        dst.x = screen_x - radius;
        dst.y = screen_y - radius;
        dst.w = radius * 2;
        dst.h = radius * 2;

        // Draw with additive blending
        SDL_RenderCopy(renderer, light_texture, nullptr, &dst);

        // Reset color mod
        SDL_SetTextureColorMod(light_texture, 255, 255, 255);
        SDL_SetTextureAlphaMod(light_texture, 255);
    }

    if (circle_effect_radius) {
        ColorConstant color = custom_color(0xceb282);
        if (circle_effect_radius <= 70) {
            color = custom_color(0xca7f5c);
        }
        auto clr = color_to_sdl(color);
        SDL_RenderFillCircle(renderer,
                             circle_effect_origin_x,
                             circle_effect_origin_y,
                             circle_effect_radius,
                             clr.r,
                             clr.g,
                             clr.b,
                             255);
    } else if (current_overlay_texture) {
        draw_overlay_layer(0);
        if (overlay_origin.y < 0) {
            draw_overlay_layer(256);
        }
    }

    draw_rect_group(0);
    draw_sprite_group(0);

    if (fade_include_overlay) {
        display_fade();
    }

    SDL_RenderPresent(renderer);
    sprite_draw_list.clear();
    rect_draw_queue.clear();
}



Vec2<u32> Platform::Screen::size() const
{
    return {240, 160};
}



void Platform::Screen::pixelate(u8 amount,
                                bool include_overlay,
                                bool include_background,
                                bool include_sprites)
{
}



////////////////////////////////////////////////////////////////////////////////
// Speaker
////////////////////////////////////////////////////////////////////////////////



struct AudioChannel
{
    const u8* data;
    StringBuffer<48> name;
    u32 length;
    u32 position;
    float volume;
    int priority;
    bool playing;
};



struct AudioState
{
    // Music channel
    const u8* music_data;
    StringBuffer<48> music_name;
    u32 music_length;
    u32 music_position;
    float music_volume;
    float music_speed;
    bool music_playing;

    // Sound effect channels (matching your GBA's active_sounds)
    static const int max_sound_channels = 8;
    AudioChannel sound_effects[max_sound_channels];

    Buffer<StringBuffer<48>, 32> completed_sounds;
    std::mutex completed_sounds_lock;

    // Stash for temporarily storing sound effects
    AudioChannel stashed_sounds[max_sound_channels];
    bool has_stashed_sounds;

    AudioState()
    {
        music_data = nullptr;
        music_length = 0;
        music_position = 0;
        music_playing = false;
        music_volume = 1.0f;
        music_speed = 1.0f;

        for (int i = 0; i < max_sound_channels; i++) {
            sound_effects[i].data = nullptr;
            sound_effects[i].length = 0;
            sound_effects[i].position = 0;
            sound_effects[i].playing = false;
            sound_effects[i].volume = 1.0f;

            stashed_sounds[i].data = nullptr;
            stashed_sounds[i].length = 0;
            stashed_sounds[i].position = 0;
            stashed_sounds[i].playing = false;
            stashed_sounds[i].volume = 1.0f;
        }

        has_stashed_sounds = false;
    }
};



static AudioState audio_state;



typedef void (*AudioMixerFunc)(AudioState* state, s8* output, int len);



void audio_mix_normal(AudioState* state, s8* output, int len)
{
    for (int i = 0; i < len; i++) {
        s16 mixed = 0;

        // Mix music
        if (state->music_playing && state->music_data) {
            if (state->music_position >= state->music_length) {
                state->music_position = 0; // Loop music
            }
            s8 sample = ((s8*)state->music_data)[state->music_position++];
            mixed = (s16)(sample * state->music_volume);
        }

        for (int ch = 0; ch < AudioState::max_sound_channels; ch++) {
            auto& sfx = state->sound_effects[ch];
            if (!sfx.playing || !sfx.data)
                continue;

            if (sfx.position >= sfx.length) {
                sfx.playing = false;
                sfx.data = nullptr;

                std::unique_lock<std::mutex> lock(state->completed_sounds_lock,
                                                  std::try_to_lock);
                if (lock.owns_lock()) {
                    state->completed_sounds.push_back(sfx.name.c_str());
                }
                continue;
            }

            s8 sample = ((s8*)sfx.data)[sfx.position++];
            mixed += (s16)(sample * sfx.volume);
        }

        // Clamp and output
        output[i] = (s8)std::clamp(mixed, (s16)-128, (s16)127);
    }
}



// Half speed - play every sample twice
void audio_mix_halved(AudioState* state, s8* output, int len)
{
    for (int i = 0; i < len; i++) {
        s16 mixed = 0;

        // Mix music
        if (state->music_playing && state->music_data) {
            if (state->music_position >= state->music_length) {
                state->music_position = 0;
            }
            // Advance position every other sample
            u32 actual_pos = state->music_position / 2;
            s8 sample = ((s8*)state->music_data)[actual_pos];
            mixed = (s16)(sample * state->music_volume);
            state->music_position++;
        }

        for (int ch = 0; ch < AudioState::max_sound_channels; ch++) {
            auto& sfx = state->sound_effects[ch];
            if (!sfx.playing || !sfx.data)
                continue;

            if (sfx.position >= sfx.length * 2) {
                sfx.playing = false;
                sfx.data = nullptr;
                std::unique_lock<std::mutex> lock(state->completed_sounds_lock,
                                                  std::try_to_lock);
                if (lock.owns_lock()) {
                    state->completed_sounds.push_back(sfx.name.c_str());
                }
                continue;
            }

            u32 actual_pos = sfx.position / 2;
            s8 sample = ((s8*)sfx.data)[actual_pos];
            mixed += (s16)(sample * sfx.volume);
            sfx.position++;
        }

        output[i] = (s8)std::clamp(mixed, (s16)-128, (s16)127);
    }
}



// Double speed - skip every other sample
void audio_mix_doubled(AudioState* state, s8* output, int len)
{
    for (int i = 0; i < len; i++) {
        s16 mixed = 0;

        // Mix music
        if (state->music_playing && state->music_data) {
            if (state->music_position >= state->music_length) {
                state->music_position = 0;
            }
            s8 sample = ((s8*)state->music_data)[state->music_position];
            mixed = (s16)(sample * state->music_volume);
            state->music_position += 2; // Skip every other sample
        }

        for (int ch = 0; ch < AudioState::max_sound_channels; ch++) {
            auto& sfx = state->sound_effects[ch];
            if (!sfx.playing || !sfx.data)
                continue;

            if (sfx.position >= sfx.length) {
                sfx.playing = false;
                sfx.data = nullptr;
                std::unique_lock<std::mutex> lock(state->completed_sounds_lock,
                                                  std::try_to_lock);
                if (lock.owns_lock()) {
                    state->completed_sounds.push_back(sfx.name.c_str());
                }
                continue;
            }

            s8 sample = ((s8*)sfx.data)[sfx.position];
            mixed += (s16)(sample * sfx.volume);
            sfx.position += 2;
        }

        output[i] = (s8)std::clamp(mixed, (s16)-128, (s16)127);
    }
}



// Reverse playback - decrement position
void audio_mix_reversed(AudioState* state, s8* output, int len)
{
    for (int i = 0; i < len; i++) {
        s16 mixed = 0;

        // Mix music
        if (state->music_playing && state->music_data) {
            if (state->music_position == 0) {
                state->music_position = state->music_length - 1;
            }
            s8 sample = ((s8*)state->music_data)[state->music_position--];
            mixed = (s16)(sample * state->music_volume);
        }

        for (int ch = 0; ch < AudioState::max_sound_channels; ch++) {
            auto& sfx = state->sound_effects[ch];
            if (!sfx.playing || !sfx.data)
                continue;

            if (sfx.position == 0) {
                sfx.playing = false;
                sfx.data = nullptr;
                continue;
            }

            s8 sample = ((s8*)sfx.data)[sfx.position--];
            mixed += (s16)(sample * sfx.volume);
        }

        output[i] = (s8)std::clamp(mixed, (s16)-128, (s16)127);
    }
}



// Reverse 4x speed
void audio_mix_reversed4x(AudioState* state, s8* output, int len)
{
    for (int i = 0; i < len; i++) {
        s16 mixed = 0;

        // Mix music
        if (state->music_playing && state->music_data) {
            if (state->music_position < 4) {
                state->music_position = state->music_length - 1;
            }
            s8 sample = ((s8*)state->music_data)[state->music_position];
            mixed = (s16)(sample * state->music_volume);
            state->music_position -= 4;
        }

        for (int ch = 0; ch < AudioState::max_sound_channels; ch++) {
            auto& sfx = state->sound_effects[ch];
            if (!sfx.playing || !sfx.data)
                continue;

            if (sfx.position < 4) {
                sfx.playing = false;
                sfx.data = nullptr;
                continue;
            }

            s8 sample = ((s8*)sfx.data)[sfx.position];
            mixed += (s16)(sample * sfx.volume);
            sfx.position -= 4;
        }

        output[i] = (s8)std::clamp(mixed, (s16)-128, (s16)127);
    }
}



// Reverse 8x speed
void audio_mix_reversed8x(AudioState* state, s8* output, int len)
{
    for (int i = 0; i < len; i++) {
        s16 mixed = 0;

        // Mix music
        if (state->music_playing && state->music_data) {
            if (state->music_position < 8) {
                state->music_position = state->music_length - 1;
            }
            s8 sample = ((s8*)state->music_data)[state->music_position];
            mixed = (s16)(sample * state->music_volume);
            state->music_position -= 8;
        }

        for (int ch = 0; ch < AudioState::max_sound_channels; ch++) {
            auto& sfx = state->sound_effects[ch];
            if (!sfx.playing || !sfx.data)
                continue;

            if (sfx.position < 8) {
                sfx.playing = false;
                sfx.data = nullptr;
                continue;
            }

            s8 sample = ((s8*)sfx.data)[sfx.position];
            mixed += (s16)(sample * sfx.volume);
            sfx.position -= 8;
        }

        output[i] = (s8)std::clamp(mixed, (s16)-128, (s16)127);
    }
}



// Reverse slow (half speed backwards)
void audio_mix_reversed_slow(AudioState* state, s8* output, int len)
{
    for (int i = 0; i < len; i++) {
        s16 mixed = 0;

        // Mix music
        if (state->music_playing && state->music_data) {
            if (state->music_position == 0) {
                state->music_position = state->music_length * 2 - 1;
            }
            // Decrement every other sample
            u32 actual_pos = state->music_position / 2;
            s8 sample = ((s8*)state->music_data)[actual_pos];
            mixed = (s16)(sample * state->music_volume);
            state->music_position--;
        }

        for (int ch = 0; ch < AudioState::max_sound_channels; ch++) {
            auto& sfx = state->sound_effects[ch];
            if (!sfx.playing || !sfx.data)
                continue;

            if (sfx.position == 0) {
                sfx.playing = false;
                sfx.data = nullptr;
                continue;
            }

            u32 actual_pos = sfx.position / 2;
            s8 sample = ((s8*)sfx.data)[actual_pos];
            mixed += (s16)(sample * sfx.volume);
            sfx.position--;
        }

        output[i] = (s8)std::clamp(mixed, (s16)-128, (s16)127);
    }
}



static AudioMixerFunc current_mixer = audio_mix_normal;



void audio_callback(void* userdata, Uint8* stream, int len)
{
    AudioState* state = (AudioState*)userdata;
    s8* output = (s8*)stream;
    current_mixer(state, output, len);
}



StringBuffer<48> Platform::Speaker::current_music()
{
    StringBuffer<48> music_name;
    SDL_LockAudio();
    music_name = audio_state.music_name;
    SDL_UnlockAudio();

    return music_name;
}



bool Platform::Speaker::stream_music(const char* filename, Microseconds offset)
{
    if (not window) {
        return true;
    }

    std::string full_path = std::string("scripts") + PATH_DELIMITER + "data" +
                            PATH_DELIMITER + "music" + PATH_DELIMITER +
                            filename;

    auto [data, length] = PLATFORM.load_file("", full_path.c_str());
    if (not data or not length) {
        error(format("failed to load %", filename));
        stop_music();
        return false;
    }

    SDL_LockAudio();

    // Stop old music
    audio_state.music_playing = false;
    audio_state.music_data = nullptr;
    audio_state.music_position = 0;

    // Now set new music
    audio_state.music_data = (const u8*)data;
    audio_state.music_length = length;
    audio_state.music_position = (offset * 16000) / 1000000;
    audio_state.music_playing = true;
    audio_state.music_name = filename;

    SDL_UnlockAudio();

    return true;
}



void Platform::Speaker::play_music(const char* name, Microseconds offset)
{
}



Platform::Speaker::Speaker()
{
}



void Platform::Speaker::set_music_speed(MusicSpeed speed)
{
    SDL_LockAudio();

    switch (speed) {
    case MusicSpeed::regular:
        current_mixer = audio_mix_normal;
        break;
    case MusicSpeed::halved:
        current_mixer = audio_mix_halved;
        break;
    case MusicSpeed::doubled:
        current_mixer = audio_mix_doubled;
        break;
    case MusicSpeed::reversed:
        current_mixer = audio_mix_reversed;
        break;
    case MusicSpeed::reversed4x:
        current_mixer = audio_mix_reversed4x;
        break;
    case MusicSpeed::reversed8x:
        current_mixer = audio_mix_reversed8x;
        break;
    case MusicSpeed::reversed_slow:
        current_mixer = audio_mix_reversed_slow;
        break;
    }

    SDL_UnlockAudio();
}



void Platform::Speaker::set_music_volume(u8 volume)
{
    // Volume ranges from 0 to 19 (music_volume_max)
    // Convert to 0.0 to 1.0 range
    float normalized_volume = volume / (float)music_volume_max;

    SDL_LockAudio();
    audio_state.music_volume = normalized_volume;
    SDL_UnlockAudio();
}



void Platform::Speaker::set_sounds_volume(u8 volume)
{
    // Volume ranges from 0 to 19 (music_volume_max)
    // Convert to 0.0 to 1.0 range
    float normalized_volume = volume / (float)music_volume_max;

    SDL_LockAudio();

    // Apply to all sound effect channels
    for (int i = 0; i < AudioState::max_sound_channels; i++) {
        audio_state.sound_effects[i].volume = normalized_volume;
    }

    SDL_UnlockAudio();
}



void Platform::Speaker::stash_sounds()
{
    SDL_LockAudio();

    // Copy all sound effect channels to the stash
    for (int i = 0; i < AudioState::max_sound_channels; i++) {
        auto& src = audio_state.sound_effects[i];
        auto& dst = audio_state.stashed_sounds[i];

        dst.data = src.data;
        dst.name = src.name;
        dst.length = src.length;
        dst.position = src.position;
        dst.volume = src.volume;
        dst.priority = src.priority;
        dst.playing = src.playing;

        // Clear the active channel
        src.playing = false;
        src.data = nullptr;
    }

    audio_state.has_stashed_sounds = true;

    SDL_UnlockAudio();
}



void Platform::Speaker::restore_sounds()
{
    SDL_LockAudio();

    if (!audio_state.has_stashed_sounds) {
        SDL_UnlockAudio();
        return;
    }

    // Restore all sound effect channels from the stash
    for (int i = 0; i < AudioState::max_sound_channels; i++) {
        auto& src = audio_state.stashed_sounds[i];
        auto& dst = audio_state.sound_effects[i];

        dst.data = src.data;
        dst.name = src.name;
        dst.length = src.length;
        dst.position = src.position;
        dst.volume = src.volume;
        dst.priority = src.priority;
        dst.playing = src.playing;

        // Clear the stash entry
        src.playing = false;
        src.data = nullptr;
    }

    audio_state.has_stashed_sounds = false;

    SDL_UnlockAudio();
}



void Platform::Speaker::start()
{
}



Microseconds Platform::Speaker::track_length(const char* name)
{
    return 1;
}



bool Platform::Speaker::is_sound_playing(const char* name)
{
    SDL_LockAudio();

    bool playing = false;
    for (int i = 0; i < AudioState::max_sound_channels; i++) {
        auto& channel = audio_state.sound_effects[i];
        if (channel.playing and channel.name == name) {
            playing = true;
            break;
        }
    }

    SDL_UnlockAudio();
    return playing;
}



bool Platform::Speaker::is_music_playing(const char* name)
{
    return current_music() == name;
}



// Use a static cache that never removes entries (strings are small)
static std::unordered_set<std::string> completed_sounds_string_cache;

const char* completed_sound_name(const char* name)
{
    // Insert returns a pair<iterator, bool>
    // The iterator always points to the element (whether newly inserted or existing)
    auto [it, inserted] = completed_sounds_string_cache.insert(name);

    // Return pointer to the string data in the set
    // This is stable because unordered_set never moves elements once inserted
    return it->c_str();
}

Buffer<const char*, 4> Platform::Speaker::completed_sounds()
{
    Buffer<const char*, 4> result;

    std::unique_lock<std::mutex> lock(audio_state.completed_sounds_lock,
                                      std::try_to_lock);
    if (lock.owns_lock()) {
        while (!audio_state.completed_sounds.empty() && result.size() < 4) {
            result.push_back(completed_sound_name(
                audio_state.completed_sounds.back().c_str()));
            audio_state.completed_sounds.pop_back();
        }
    }

    return result;
}


void Platform::Speaker::stop_sound(const char* name)
{
    SDL_LockAudio();

    for (int i = 0; i < AudioState::max_sound_channels; i++) {
        auto& channel = audio_state.sound_effects[i];
        if (channel.playing and channel.name == name) {
            channel.playing = false;
        }
    }

    SDL_UnlockAudio();
}



void Platform::Speaker::clear_sounds()
{
    SDL_LockAudio();

    for (int i = 0; i < AudioState::max_sound_channels; i++) {
        auto& channel = audio_state.sound_effects[i];
        channel.playing = false;
    }

    SDL_UnlockAudio();
}



void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   Optional<Vec2<Float>> position)
{
    if (not window) {
        return;
    }

    const char* data = nullptr;
    u32 length = 0;

    // Try option 1: sounds/sound_<name>.raw
    std::string path1 =
        std::string("sounds") + PATH_DELIMITER + "sound_" + name + ".raw";
    auto result1 = PLATFORM.load_file("", path1.c_str());

    if (result1.first && result1.second) {
        data = result1.first;
        length = result1.second;
    } else {
        // Try option 2: scripts/data/sounds/<name> (no extension)
        std::string path2 = std::string("scripts") + PATH_DELIMITER + "data" +
                            PATH_DELIMITER + "sounds" + PATH_DELIMITER + name;
        auto result2 = PLATFORM.load_file("", path2.c_str());

        if (result2.first && result2.second) {
            data = result2.first;
            length = result2.second;
        } else {
            warning(format("Failed to load sound: % (tried % and %)",
                           name,
                           path1.c_str(),
                           path2.c_str()));
            return;
        }
    }

    SDL_LockAudio();

    // Find a free channel or replace lowest priority
    int target_channel = -1;
    int lowest_priority_channel = 0;
    int lowest_priority = INT_MAX;

    for (int i = 0; i < AudioState::max_sound_channels; i++) {
        if (!audio_state.sound_effects[i].playing) {
            target_channel = i;
            break;
        }
        // Track lowest priority for potential replacement
        if (audio_state.sound_effects[i].priority < lowest_priority) {
            lowest_priority = audio_state.sound_effects[i].priority;
            lowest_priority_channel = i;
        }
    }

    // If no free channel, check if we can evict a lower priority sound
    if (target_channel == -1) {
        if (priority > lowest_priority) {
            target_channel = lowest_priority_channel;
        } else {
            SDL_UnlockAudio();
            return; // Can't play this sound, all channels busy with higher priority
        }
    }

    // Set up the channel
    auto& channel = audio_state.sound_effects[target_channel];
    channel.data = (const u8*)data;
    channel.length = length;
    channel.position = 0;
    if (current_mixer == audio_mix_reversed or
        current_mixer == audio_mix_reversed4x or
        current_mixer == audio_mix_reversed8x or
        current_mixer == audio_mix_reversed_slow) {
        // Note: if we're playing a sound from the rewind logic, naturally we'll
        // want to initiate the song from the final sample, as it'll be playing
        // in reverse. You could certainly set the position to zero, but then
        // nothing would play...
        channel.position = channel.length - 1;
    }
    channel.playing = true;
    channel.volume = 1.0f;
    channel.priority = priority;
    channel.name = name; // Note: assumes name string persists

    SDL_UnlockAudio();
}



void Platform::Speaker::stop_music()
{
    SDL_LockAudio();

    audio_state.music_playing = false;
    audio_state.music_data = nullptr;
    audio_state.music_position = 0;
    audio_state.music_name.clear();

    SDL_UnlockAudio();
}



void initialize_audio()
{
    SDL_AudioSpec desired;
    desired.freq = 16000;
    desired.format = AUDIO_S8;
    desired.channels = 1;
    desired.samples = 1024;
    desired.callback = audio_callback;
    desired.userdata = &audio_state;

    if (SDL_OpenAudio(&desired, nullptr) < 0) {
        // NOTE: nullptr in obtained paramter forces SDL to give us the audio
        // encoding that we want. Otherwise, the sound on some systems will
        // resemble a distorted electric guitar... wrong sample encoding.
        error(format("Failed to open audio: %", SDL_GetError()));
    }

    SDL_PauseAudio(0);
}



////////////////////////////////////////////////////////////////////////////////
// Platform impl
////////////////////////////////////////////////////////////////////////////////



Platform::Platform()
{
    update_viewport();
    constrain_window_to_scale();

    initialize_audio();

    std::ifstream in(get_save_file_path(),
                     std::ios_base::in | std::ios_base::binary);
    if (in) {
        in.read((char*)save_buffer, ::save_capacity);
    }
}



Platform::~Platform()
{
    server_running = false;

    if (server_socket != INVALID_SOCKET) {
        CLOSE_SOCKET(server_socket);
        server_socket = INVALID_SOCKET;
    }

    if (server_thread.joinable()) {
        server_thread.join();
    }

    cleanup_point_light_cache();
    cleanup_charset_surfaces();
}



bool Platform::is_running() const
{
    return sdl_running;
}



Platform::DeviceName Platform::device_name() const
{
    return "PC";
}



Platform::ModelName Platform::model_name() const
{
#if defined(_WIN32) or defined(_WIN64)
    return "Windows";
#elif defined(__APPLE__)
    return "Mac";
#else
    return "Linux";
#endif
}



void Platform::memset_words(void* dest, u8 byte, u32 word_count)
{
    memset(dest, byte, word_count * sizeof(void*));
}



void no_op_task()
{
}


Platform::TaskPointer Platform::set_background_task(Platform::TaskPointer task)
{
    return no_op_task;
}
