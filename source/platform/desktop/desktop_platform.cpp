////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "number/random.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"


////////////////////////////////////////////////////////////////////////////////
//
//
// Desktop Platform
//
//
// FIXME: I really do intend to clean this file up. These platform
// implementations contain some of the most garbage code in the codebase. I just
// can't be bothered to update the console platform implementations, because
// consoles are frozen hardware that never changes, and I can't be bothered to
// clean up the sfml desktop implementation here, because SFML is dying and I
// really just need to replace this code with SDL.
//
//
////////////////////////////////////////////////////////////////////////////////


#ifdef _WIN32
#define PATH_DELIMITER "\\"
#else
#define PATH_DELIMITER "/"
#endif


#include "SFML/Audio.hpp"
#include "SFML/Graphics.hpp"
#include "SFML/Network.hpp"
#include "SFML/System.hpp"
#include <chrono>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <list>
// The game logic and graphics used to run on different threads. But the game is
// efficient enough to run on a gameboy, so there isn't really any need for
// threading.
// #include <mutex>
#include <popl/popl.hpp>
#include <queue>
#include <sstream>
#include <thread>
#include <unordered_map>


Platform::DeviceName Platform::device_name() const
{
    return "PC";
}



void Platform::restart()
{
    while (true)
        ;
}



std::string resource_path();



int save_capacity = 32000;



u8 save_buffer[32000];



int Platform::save_capacity()
{
    return ::save_capacity;
}



////////////////////////////////////////////////////////////////////////////////
// TileMap
////////////////////////////////////////////////////////////////////////////////


class TileMap : public sf::Drawable, public sf::Transformable
{
public:
    TileMap(sf::Texture* texture, sf::Vector2u tile_size, int width, int height)
        : texture_(texture), tile_size_(tile_size), width_(width),
          height_(height)
    {
        // resize the vertex array to fit the level size
        vertices_.setPrimitiveType(sf::Quads);
        vertices_.resize(width * height * 4);
    }

    void set_tilesize(unsigned size)
    {
        for (int x = 0; x < width_; ++x) {
            for (int y = 0; y < height_; ++y) {
                set_tile(x, y, 0);
            }
        }
        tile_size_ = {size, size};
        for (int x = 0; x < width_; ++x) {
            for (int y = 0; y < height_; ++y) {
                set_tile(x, y, 0);
            }
        }
    }

    void set_tile(int x, int y, int index)
    {
        if (x >= width_ or y >= height_) {
            return;
        }

        if (texture_->getSize().x == 0 or texture_->getSize().y == 0 or
            tile_size_.x == 0 or tile_size_.y == 0) {
            return;
        }

        // find tile position in the tileset texture
        int tu = index % (texture_->getSize().x / tile_size_.x);
        int tv = index / (texture_->getSize().x / tile_size_.x);

        // get a pointer to the current tile's quad
        sf::Vertex* quad = &vertices_[(x + y * width_) * 4];

        // define its 4 corners
        quad[0].position = sf::Vector2f(x * tile_size_.x, y * tile_size_.y);
        quad[1].position =
            sf::Vector2f((x + 1) * tile_size_.x, y * tile_size_.y);
        quad[2].position =
            sf::Vector2f((x + 1) * tile_size_.x, (y + 1) * tile_size_.y);
        quad[3].position =
            sf::Vector2f(x * tile_size_.x, (y + 1) * tile_size_.y);

        // define its 4 texture coordinates
        quad[0].texCoords = sf::Vector2f(tu * tile_size_.x, tv * tile_size_.y);
        quad[1].texCoords =
            sf::Vector2f((tu + 1) * tile_size_.x, tv * tile_size_.y);
        quad[2].texCoords =
            sf::Vector2f((tu + 1) * tile_size_.x, (tv + 1) * tile_size_.y);
        quad[3].texCoords =
            sf::Vector2f(tu * tile_size_.x, (tv + 1) * tile_size_.y);
    }

    Vec2<int> size() const
    {
        return {width_, height_};
    }

private:
    virtual void draw(sf::RenderTarget& target, sf::RenderStates states) const
    {
        states.transform *= getTransform();

        states.texture = texture_;

        target.draw(vertices_, states);
    }

    sf::VertexArray vertices_;
    sf::Texture* texture_;
    sf::Vector2u tile_size_;
    const int width_;
    const int height_;
};


////////////////////////////////////////////////////////////////////////////////
// Global State Data
////////////////////////////////////////////////////////////////////////////////


static constexpr Vec2<u32> resolution{240, 160};


static const TileDesc glyph_region_start = 504;


class Platform::Data
{
public:
    sf::Texture spritesheet_texture_;
    sf::Texture tile0_texture_;
    sf::Texture tile1_texture_;
    sf::Texture overlay_texture_;
    sf::Texture background_texture_;
    sf::Shader color_shader_;

    sf::Image current_overlay_image_;
    sf::Image current_tile0_image_;
    sf::Image current_tile1_image_;

    sf::Image character_source_image_;

    using GlyphOffset = int;

    std::map<GlyphOffset, TileDesc> glyph_table_;
    TileDesc next_glyph_ = glyph_region_start;

    Vec2<Float> overlay_origin_;

    sf::RectangleShape fade_overlay_;
    bool fade_include_sprites_ = true;
    bool fade_include_overlay_ = false;

    sf::Color fade_color_ = sf::Color::Transparent;

    TileMap overlay_;
    TileMap map_0_[6];
    TileMap map_1_[6];
    TileMap background_;

    int depth_0_[64][64];
    int depth_1_[64][64];

    bool map_0_changed_ = false;
    bool map_1_changed_ = false;
    bool background_changed_ = false;

    int map_0_xscroll_ = 0;
    int map_0_yscroll_ = 0;

    int map_1_xscroll_ = 0;
    int map_1_yscroll_ = 0;

    sf::RenderTexture map_0_rt_;
    sf::RenderTexture map_1_rt_;
    sf::RenderTexture background_rt_;

    const bool fullscreen_;
    int window_scale_ = 2;
    sf::RenderWindow window_;
    sf::RenderTexture rt_;

    Vec2<u32> window_size_;

    // std::mutex audio_lock_;
    sf::Music music_;
    std::list<sf::Sound> sounds_;
    std::map<std::string, sf::SoundBuffer> sound_data_;


    Data()
        : overlay_(&overlay_texture_, {8, 8}, 32, 32),
          map_0_{
              {&tile0_texture_, {8, 8}, 64, 32},
              {&tile0_texture_, {8, 8}, 64, 32},
              {&tile0_texture_, {8, 8}, 64, 32},
              {&tile0_texture_, {8, 8}, 64, 32},
              {&tile0_texture_, {8, 8}, 64, 32},
              {&tile0_texture_, {8, 8}, 64, 32},
          },
          map_1_{
              {&tile1_texture_, {8, 8}, 64, 32},
              {&tile1_texture_, {8, 8}, 64, 32},
              {&tile1_texture_, {8, 8}, 64, 32},
              {&tile1_texture_, {8, 8}, 64, 32},
              {&tile1_texture_, {8, 8}, 64, 32},
              {&tile1_texture_, {8, 8}, 64, 32},
          },
          background_(&background_texture_, {8, 8}, 32, 32),
          fullscreen_( // lisp::loadv<lisp::Integer>("fullscreen").value_
              false),
          window_scale_([&] {
              if (fullscreen_) {
                  auto ssize = sf::VideoMode::getDesktopMode();
                  auto x_scale = ssize.width / resolution.x;
                  auto y_scale = ssize.height / resolution.y;
                  return static_cast<int>(std::max(x_scale, y_scale));
              } else {
                  return
                      // lisp::loadv<lisp::Integer>("window-scale").value_
                      2;
              }
          }()),
          window_(sf::VideoMode(resolution.x * window_scale_,
                                resolution.y * window_scale_),
                  "Skyland",
                  [&] {
                      if (fullscreen_) {
                          return static_cast<int>(sf::Style::Fullscreen);
                      } else {
                          return sf::Style::Titlebar | sf::Style::Close |
                                 sf::Style::Resize;
                      }
                  }())
    {
        window_.setVerticalSyncEnabled(true);
        // window_.setMouseCursorVisible(false);

        memset(depth_0_, 0, sizeof depth_0_);
        memset(depth_1_, 0, sizeof depth_1_);

        window_size_ = {(u32)resolution.x * window_scale_,
                        (u32)resolution.y * window_scale_};

        rt_.create(resolution.x, resolution.y);

        auto sound_folder = resource_path() + ("sounds" PATH_DELIMITER);
        // lisp::loadv<lisp::Symbol>("sound-dir").name_;

        for (auto& dirent : std::filesystem::directory_iterator(sound_folder)) {
            const auto filename = dirent.path().stem().string();
            static const std::string prefix("sound_");
            const auto prefix_loc = filename.find(prefix);

            if (prefix_loc not_eq std::string::npos) {
                std::ifstream file_data(dirent.path(), std::ios::binary);
                std::vector<s8> buffer(
                    std::istreambuf_iterator<char>(file_data), {});

                // The gameboy advance sound data was 8 bit signed mono at
                // 16kHz. Here, we're upsampling to 16bit signed.
                std::vector<s16> upsampled;

                for (s8 sample : buffer) {
                    upsampled.push_back(sample << 8);
                }

                sf::SoundBuffer sound_buffer;
                sound_buffer.loadFromSamples(
                    upsampled.data(), upsampled.size(), 1, 16000);

                sound_data_[filename.substr(prefix.size())] = sound_buffer;
            }
        }

        auto image_folder = resource_path() + ("images" PATH_DELIMITER);

        const auto charset_path = std::string(image_folder) + "charset.png";
        if (not character_source_image_.loadFromFile(charset_path)) {
            exit(EXIT_FAILURE);
        }
    }
};


Platform* __platform__ = nullptr;



Platform& Platform::instance()
{
    return *__platform__;
}



////////////////////////////////////////////////////////////////////////////////
// DeltaClock
////////////////////////////////////////////////////////////////////////////////


Platform::DeltaClock::DeltaClock() : impl_(new sf::Clock)
{
}



Platform::DeltaClock::TimePoint Platform::DeltaClock::sample() const
{
    return 1;
}


std::chrono::time_point throttle_start =
    std::chrono::high_resolution_clock::now();
std::chrono::time_point throttle_stop =
    std::chrono::high_resolution_clock::now();
static int sleep_time;


static Microseconds last_delta = 1;



Microseconds Platform::DeltaClock::last_delta() const
{
    return ::last_delta;
}



Microseconds Platform::DeltaClock::reset()
{
    // NOTE: I originally developed the game on the nintendo gameboy
    // advance. The game was designed to use delta timing, but stuff still seems
    // to break when running on modern hardware, which has a WAY faster clock
    // than the gameboy. So for now, I'm intentionally slowing things
    // down. Throttling the game logic also saves battery life.

    throttle_stop = std::chrono::high_resolution_clock::now();


#if not defined(__linux__) and not defined(_WIN32)
    // Unfortunately, this code seems to make the linux builds
    // really stuttery. Without this enabled, you're likely to see
    // high cpu usage.
    const auto gba_fixed_step = 2000;
    const auto elapsed = std::chrono::duration_cast<std::chrono::microseconds>(
        throttle_stop - throttle_start);

    if (elapsed.count() < gba_fixed_step) {
        std::this_thread::sleep_for(std::chrono::microseconds(
            (gba_fixed_step - 1000) - (elapsed.count() - sleep_time)));
    }
#endif

    auto val = reinterpret_cast<sf::Clock*>(impl_)->restart().asMicroseconds();

    val -= sleep_time;
    sleep_time = 0;

    throttle_start = std::chrono::high_resolution_clock::now();
    ;


    // On the gameboy advance, I was returning a fixed step based on the screen
    // refresh rate, with I thought was 60Hz. But, in fact, the screen refresh
    // rate on the gameboy was 59.59, so we'll have to scale the delta time to
    // get things to run correctly.
    constexpr float scaling_factor = (60.f / 59.59f);

    ::last_delta = val * scaling_factor;
    return ::last_delta;
}


Platform::DeltaClock::~DeltaClock()
{
    delete reinterpret_cast<sf::Clock*>(impl_);
}


////////////////////////////////////////////////////////////////////////////////
// Keyboard
////////////////////////////////////////////////////////////////////////////////

// std::mutex event_queue_lock;
std::queue<sf::Event> event_queue;


std::array<sf::Keyboard::Key, static_cast<int>(Key::count)> keymap;


void Platform::Keyboard::rumble(bool enabled)
{
    // TODO... SFML doesn't support joystick rumble yet.
}


void Platform::Keyboard::poll()
{
    // FIXME: Poll is now called from the logic thread, which means that the
    // graphics loop needs to window enqueue events in a synchronized buffer,
    // and then the logic thread will read events from the buffer and process
    // them.

    for (size_t i = 0; i < size_t(Key::count); ++i) {
        prev_[i] = states_[i];
    }

    static bool focused_ = true;

    // std::lock_guard<std::mutex> guard(::event_queue_lock);
    while (not event_queue.empty()) {
        auto event = event_queue.front();
        event_queue.pop();

        switch (event.type) {
        case sf::Event::LostFocus:
            focused_ = false;
            break;

        case sf::Event::GainedFocus:
            focused_ = true;
            break;

        case sf::Event::Closed:
            PLATFORM.data()->window_.close();
            break;

        case sf::Event::KeyPressed:
            if (not focused_) {
                break;
            }
            for (int keycode = 0; keycode < static_cast<int>(Key::count);
                 ++keycode) {
                if (keymap[keycode] == event.key.code) {
                    states_[keycode] = true;
                }
            }
            break;

        case sf::Event::KeyReleased:
            if (not focused_) {
                break;
            }
            for (int keycode = 0; keycode < static_cast<int>(Key::count);
                 ++keycode) {
                if (keymap[keycode] == event.key.code) {
                    states_[keycode] = false;
                }
            }
            break;

        // FIXME: It may be undefined behavior to handle joystick events on the
        // non-main thread...
        case sf::Event::JoystickConnected: {
            info(("joystick " +
                  std::to_string(event.joystickConnect.joystickId) +
                  " connected")
                     .c_str());
            break;
        }

        case sf::Event::JoystickDisconnected: {
            info(("joystick " +
                  std::to_string(event.joystickConnect.joystickId) +
                  " disconnected")
                     .c_str());
            break;
        }

        case sf::Event::JoystickButtonPressed: {
            break;
        }

        case sf::Event::JoystickButtonReleased: {
            break;
        }

        case sf::Event::JoystickMoved: {
            break;
        }

        default:
            break;
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Screen
////////////////////////////////////////////////////////////////////////////////


Platform::Screen::Screen()
{
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



const Platform::Screen::Touch* Platform::Screen::touch() const
{
    return nullptr;
}



Vec2<u32> Platform::Screen::size() const
{
    const auto data = PLATFORM.data();
    return {data->window_size_.x / data->window_scale_,
            data->window_size_.y / data->window_scale_};
}


enum class TextureSwap { spritesheet, tile0, tile1, overlay };


static std::queue<std::pair<TextureSwap, std::string>> texture_swap_requests;
static std::queue<std::pair<TileDesc, Platform::TextureMapping>> glyph_requests;



static ObjectPool<PooledRcControlBlock<Platform::DynamicTexture,
                                       Platform::dynamic_texture_count>,
                  Platform::dynamic_texture_count>
    dynamic_texture_pool("dynamic-texture-pool");



void Platform::DynamicTexture::remap(u16 spritesheet_offset)
{
    // No need to do anything on the desktop platform. All of our texture fits
    // in vram, so we do not need to manage any "virtual" tile indices.
}



static struct DynamicTextureMapping
{
    bool reserved_ = false;
    bool dirty_ = false;
    u16 spritesheet_offset_ = 0;
} dynamic_texture_mappings[Platform::dynamic_texture_count];



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

    warning("Failed to allocate DynamicTexture.");
    return {};
}



void Platform::Screen::clear()
{
    auto& window = PLATFORM.data()->window_;
    auto& rt = PLATFORM.data()->rt_;
    window.clear(sf::Color(95, 168, 234));
    rt.clear(sf::Color(95, 168, 234));

    PLATFORM.data()->fade_overlay_.setFillColor(PLATFORM.data()->fade_color_);

    {
        auto& sounds = PLATFORM.data()->sounds_;
        for (auto it = sounds.begin(); it not_eq sounds.end();) {
            if (it->getStatus() == sf::Sound::Status::Stopped) {
                it = sounds.erase(it);
            } else {
                ++it;
            }
        }
    }

    {
        while (not texture_swap_requests.empty()) {
            const auto request = texture_swap_requests.front();
            texture_swap_requests.pop();

            sf::Image image;

            auto image_folder = resource_path() + ("images" PATH_DELIMITER);

            if (not image.loadFromFile(image_folder + request.second +
                                       ".png")) {
                error((std::string("failed to load texture ") + request.second)
                          .c_str());
                exit(EXIT_FAILURE);
            } else {
                info((std::string("loaded image ") + request.second).c_str());
            }
            image.createMaskFromColor({255, 0, 255, 255});

            if (request.first == TextureSwap::overlay) {
                PLATFORM.data()->current_overlay_image_.create(
                    image.getSize().x, image.getSize().y);
                PLATFORM.data()->current_overlay_image_.copy(
                    image,
                    0,
                    0,
                    {0, 0, (int)image.getSize().x, (int)image.getSize().y});
            }

            if (image.getSize().x > 4032) {
                sf::Image replacement;
                replacement.create(4032, image.getSize().y);
                replacement.copy(
                    image, 0, 0, {0, 0, 4032, (int)image.getSize().y});
                std::swap(image, replacement);
            }

            if (not [&] {
                    switch (request.first) {
                    case TextureSwap::spritesheet:
                        return &PLATFORM.data()->spritesheet_texture_;

                    case TextureSwap::tile0:
                        return &PLATFORM.data()->tile0_texture_;

                    case TextureSwap::tile1:
                        return &PLATFORM.data()->tile1_texture_;

                    case TextureSwap::overlay:
                        return &PLATFORM.data()->overlay_texture_;
                    }
                    error("invalid texture swap enumeration");
                    PLATFORM.fatal("invalid texture swap enumeration");
                }()
                        ->loadFromImage(image)) {
                error("Failed to create texture");
                exit(EXIT_FAILURE);
            }
        }
    }


    {
        // std::lock_guard<std::mutex> guard(::event_queue_lock);

        // This is really hacky and bad. We can't check for keypresses on the
        // other thread, and the sfml window does not fire keypressed events
        // consistently while a key is held down, so we're generating our own
        // events.
        for (int keycode = 0; keycode < static_cast<int>(Key::count);
             ++keycode) {
            sf::Event event;
            if (sf::Keyboard::isKeyPressed(keymap[keycode])) {
                event.type = sf::Event::KeyPressed;
                event.key.code = keymap[keycode];
            } else {
                event.type = sf::Event::KeyReleased;
                event.key.code = keymap[keycode];
            }
            ::event_queue.push(event);
        }


        sf::Event event;
        while (window.pollEvent(event)) {
            switch (event.type) {
            case sf::Event::KeyPressed:
            case sf::Event::KeyReleased:
                // We're generating our own events, see above.
                break;

            default:
                ::event_queue.push(event);
            }
        }
    }
}


std::vector<Sprite> draw_queue;


static sf::Glsl::Vec3 make_color(int color_hex)
{
    const auto r = (color_hex & 0xFF0000) >> 16;
    const auto g = (color_hex & 0x00FF00) >> 8;
    const auto b = (color_hex & 0x0000FF);

    return {r / 255.f, g / 255.f, b / 255.f};
}


static sf::Glsl::Vec3 real_color(ColorConstant k)
{
    return make_color(static_cast<int>(k));
}


sf::View get_letterbox_view(sf::View view, int window_width, int window_height)
{

    // Compares the aspect ratio of the window to the aspect ratio of the view,
    // and sets the view's viewport accordingly in order to archieve a letterbox effect.
    // A new view (with a new viewport set) is returned.

    float window_ratio = window_width / (float)window_height;
    float view_ratio = view.getSize().x / (float)view.getSize().y;
    float size_x = 1;
    float size_y = 1;
    float pos_x = 0;
    float pos_y = 0;

    bool horizontal_spacing = true;
    if (window_ratio < view_ratio)
        horizontal_spacing = false;

    // If horizontalSpacing is true, the black bars will appear on the left and right side.
    // Otherwise, the black bars will appear on the top and bottom.

    if (horizontal_spacing) {
        size_x = view_ratio / window_ratio;
        pos_x = (1 - size_x) / 2.f;
    }

    else {
        size_y = window_ratio / view_ratio;
        pos_y = (1 - size_y) / 2.f;
    }

    view.setViewport(sf::FloatRect(pos_x, pos_y, size_x, size_y));

    return view;
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



void Platform::Screen::display()
{
    view_.set_size(size().cast<Float>());
    sf::View view;
    view.setSize(view_.get_size().x, view_.get_size().y);

    const bool fade_overlay = PLATFORM.data()->fade_include_overlay_;


    sf::View fixed_view;
    fixed_view = get_letterbox_view(view,
                                    PLATFORM.data()->window_.getSize().x,
                                    PLATFORM.data()->window_.getSize().y);

    fixed_view.setCenter({view_.get_size().x / 2, view_.get_size().y / 2});

    auto& window = PLATFORM.data()->window_;
    window.setView(fixed_view);

    ////////////////////////////////////////////////////////////////////////////
    //
    // Draw map layer 1
    //

    if (PLATFORM.data()->map_1_changed_) {
        PLATFORM.data()->map_1_changed_ = false;
        PLATFORM.data()->map_1_rt_.clear(sf::Color::Transparent);
        PLATFORM.data()->map_1_rt_.draw(PLATFORM.data()->map_1_[0]);
        // for (int i = 0; i < 6; ++i) {
        //     PLATFORM.data()->map_0_rt_.draw(PLATFORM.data()->map_0_[i]);
        // }
        PLATFORM.data()->map_1_rt_.display();
    }

    sf::Sprite map1(PLATFORM.data()->map_1_rt_.getTexture());
    map1.setPosition(
        -(PLATFORM.data()->map_1_xscroll_ + view_.get_center().x),
        wrap_y(-(PLATFORM.data()->map_1_yscroll_ + view_.get_center().y)));
    window.draw(map1);


    ////////////////////////////////////////////////////////////////////////////
    //
    // Draw map layer 0
    //

    if (PLATFORM.data()->map_0_changed_) {
        PLATFORM.data()->map_0_changed_ = false;
        PLATFORM.data()->map_0_rt_.clear(sf::Color::Transparent);
        PLATFORM.data()->map_0_rt_.draw(PLATFORM.data()->map_0_[0]);
        // for (int i = 0; i < 6; ++i) {
        //     PLATFORM.data()->map_0_rt_.draw(PLATFORM.data()->map_0_[i]);
        // }
        PLATFORM.data()->map_0_rt_.display();
    }

    sf::Sprite map0(PLATFORM.data()->map_0_rt_.getTexture());
    int m0_xscroll = -(PLATFORM.data()->map_0_xscroll_ + view_.get_center().x);
    int m0_yscroll = -(PLATFORM.data()->map_0_yscroll_ + view_.get_center().y);
    m0_yscroll = wrap_y(m0_yscroll);
    // std::cout << m0_yscroll << std::endl;
    map0.setPosition(m0_xscroll, m0_yscroll);
    window.draw(map0);


    if (not fade_overlay) {
        window.draw(PLATFORM.data()->fade_overlay_);
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    // Draw sprites
    //

    for (auto& spr : reversed(draw_queue)) {
        if (spr.get_alpha() == Sprite::Alpha::transparent) {
            continue;
        }

        const auto& pos = spr.get_position();
        const auto& flip = spr.get_flip();

        sf::Sprite sf_spr;
        if (auto rot = spr.get_rotation()) {
            sf_spr.setRotation((float(rot) / std::numeric_limits<s16>::max()) *
                               360);
        }

        sf_spr.setPosition({pos.x.as_float() - view_.get_center().x,
                            wrap_y(pos.y.as_float() - view_.get_center().y)});

        sf_spr.setScale({flip.x ? -1.f : 1.f, flip.y ? -1.f : 1.f});
        sf_spr.setTexture(PLATFORM.data()->spritesheet_texture_);

        switch (const auto ind = spr.get_texture_index(); spr.get_size()) {
        default: // FIXME...
        case Sprite::Size::w16_h16: {
            int y = 0;
            if (ind % 2 == 1) {
                y = 16;
            }
            sf_spr.setTextureRect({static_cast<s32>(ind / 2) * 16, y, 16, 16});
            break;
        }

        case Sprite::Size::w16_h32:
            sf_spr.setTextureRect({static_cast<s32>(ind) * 16, 0, 16, 32});
            break;

        case Sprite::Size::w32_h32:
            sf_spr.setTextureRect({static_cast<s32>(ind) * 32, 0, 32, 32});
            break;
        }

        if (spr.get_alpha() == Sprite::Alpha::translucent) {
            sf_spr.setColor({255, 255, 255, 127});
        }

        if (const auto& mix = spr.get_mix();
            mix.color_ not_eq ColorConstant::null) {
            sf::Shader& shader = PLATFORM.data()->color_shader_;
            shader.setUniform("amount", mix.amount_ / 255.f);
            shader.setUniform("targetColor", real_color(mix.color_));
            window.draw(sf_spr, &shader);
        } else {
            window.draw(sf_spr);
        }
    }


    ////////////////////////////////////////////////////////////////////////////
    //
    // Draw overlay
    //

    auto& origin = PLATFORM.data()->overlay_origin_;
    fixed_view.setCenter(
        {view_.get_size().x / 2 + origin.x, view_.get_size().y / 2 + origin.y});

    window.setView(fixed_view);
    window.draw(PLATFORM.data()->overlay_);


    fixed_view.setCenter({view_.get_size().x / 2, view_.get_size().y / 2});

    window.setView(fixed_view);

    if (fade_overlay) {
        window.draw(PLATFORM.data()->fade_overlay_);
    }

    window.display();

    draw_queue.clear();
}



static bool fade_is_active = false;



void Platform::Screen::fade(Float amount,
                            ColorConstant k,
                            Optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{
    const auto c = real_color(k);

    fade_is_active = amount > 0;

    if (not base) {
        PLATFORM.data()->fade_color_ = {static_cast<uint8_t>(c.x * 255),
                                        static_cast<uint8_t>(c.y * 255),
                                        static_cast<uint8_t>(c.z * 255),
                                        static_cast<uint8_t>(amount * 255)};
        PLATFORM.data()->fade_include_sprites_ = include_sprites;
        PLATFORM.data()->fade_include_overlay_ = include_overlay;
    } else {
        const auto c2 = real_color(*base);
        PLATFORM.data()->fade_color_ = {
            interpolate(static_cast<uint8_t>(c.x * 255),
                        static_cast<uint8_t>(c2.x * 255),
                        amount),
            interpolate(static_cast<uint8_t>(c.y * 255),
                        static_cast<uint8_t>(c2.y * 255),
                        amount),
            interpolate(static_cast<uint8_t>(c.z * 255),
                        static_cast<uint8_t>(c2.z * 255),
                        amount),
            static_cast<uint8_t>(255)};
    }
}



bool Platform::Screen::fade_active() const
{
    return fade_is_active;
}



void Platform::Screen::schedule_fade(Float amount,
                                     ColorConstant k,
                                     bool include_sprites,
                                     bool include_overlay,
                                     bool include_background,
                                     bool include_tiles,
                                     bool dodge)
{
    fade(amount, k, {}, include_sprites or not dodge, include_overlay);
}


void Platform::Screen::pixelate(u8 amount,
                                bool include_overlay,
                                bool include_background,
                                bool include_sprites)
{
    // TODO... Need to work on the shader pipeline...
}


void Platform::Screen::draw(const Sprite& spr)
{
    draw_queue.push_back(spr);
}



void Platform::Screen::draw_batch(TextureIndex t,
                                  const Buffer<Vec2<s32>, 64>& coords,
                                  const SpriteBatchOptions& opts)
{
}



////////////////////////////////////////////////////////////////////////////////
// Speaker
////////////////////////////////////////////////////////////////////////////////



Buffer<const char*, 4> Platform::Speaker::completed_sounds()
{
    return {};
}



bool Platform::Speaker::is_sound_playing(const char* name)
{
    return true;
}



void Platform::Speaker::set_sounds_volume(u8 vol)
{
}



void Platform::Speaker::stop_sound(const char* name)
{
}



void Platform::Speaker::clear_sounds()
{
}



void Platform::Speaker::stash_sounds()
{
}



void Platform::Speaker::restore_sounds()
{
}



void Platform::Speaker::set_music_volume(u8 volume)
{
}



void Platform::Speaker::set_music_speed(MusicSpeed speed)
{
}


Platform::Speaker::Speaker()
{
    // TODO...
}



void Platform::Speaker::start()
{
}



void Platform::Speaker::init_chiptune_square_1(ChannelSettings settings)
{
}



void Platform::Speaker::init_chiptune_square_2(ChannelSettings settings)
{
}



void Platform::Speaker::init_chiptune_wave(u16 config)
{
}



void Platform::Speaker::init_chiptune_noise(ChannelSettings settings)
{
}



void Platform::Speaker::stop_chiptune_note(Channel channel)
{
}



void Platform::Speaker::play_chiptune_note(Channel channel, NoteDesc note_desc)
{
}



void Platform::Speaker::apply_chiptune_effect(Channel channel,
                                              Effect effect,
                                              u8 argument,
                                              Microseconds delta)
{
}



static std::string current_music;


void Platform::Speaker::play_music(const char* name, Microseconds offset)
{
    std::string path = resource_path() + ("sounds" PATH_DELIMITER);

    path += "music_";
    path += name;
    path += ".ogg";

    if (PLATFORM.data()->music_.openFromFile(path.c_str())) {
        PLATFORM.data()->music_.play();
        PLATFORM.data()->music_.setLoop(true);
        PLATFORM.data()->music_.setPlayingOffset(sf::microseconds(offset));
        // For desktop releases, we have spatialized sound enabled, such that
        // the sound effects attenuate based on their distance from the center
        // of the screen. The music seems too loud by comparison, so I'm
        // playing the music more quietly than I would on the gameboy advance,
        // where everthing is mono (there's only one speaker on the device, so
        // stereo isn't really worth the resources on the gameboy).
        PLATFORM.data()->music_.setVolume(70);

        ::current_music = name;

    } else {
        error("failed to load music file");
    }
}


void Platform::Speaker::stop_music()
{
    PLATFORM.data()->music_.stop();
}


void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   Optional<Vec2<Float>> position)
{
    (void)priority; // We are not using the priority variable, because we're a
                    // powerful desktop pc, we can play lots of sounds at once,
                    // and do not need to evict sounds! Or do we...? I think
                    // SFML does have a hard upper limit of 256 concurrent audio
                    // streams, but we'll probably never hit that limit, given
                    // that the game was originally calibrated for the gameboy
                    // advance, where I was supporting four concurrent audio
                    // streams.

    // std::lock_guard<std::mutex> guard(PLATFORM.data()->audio_lock_);
    auto& data = PLATFORM.data()->sound_data_;
    auto found = data.find(name);
    if (found not_eq data.end()) {
        PLATFORM.data()->sounds_.emplace_back(found->second);

        auto& sound = PLATFORM.data()->sounds_.back();

        sound.play();

        // sound.setRelativeToListener(static_cast<bool>(position));

        // if (position) {
        //     sound.setAttenuation(0.2f);
        // } else {
        //     sound.setAttenuation(0.f);
        // }

        // sound.setMinDistance(160.f);
        // if (position) {
        //     sound.setPosition({position->x, 0, position->y});
        // }
    } else {
        error((std::string("no sound data for ") + name).c_str());
    }
}


bool is_sound_playing(const char* name)
{
    return false; // TODO...
}


bool Platform::Speaker::is_music_playing(const char* name)
{
    return ::current_music == name;
}


////////////////////////////////////////////////////////////////////////////////
// RemoteConsole
////////////////////////////////////////////////////////////////////////////////



void Platform::RemoteConsole::start()
{
}



auto Platform::RemoteConsole::readline() -> Optional<Line>
{
    // TODO...
    return {};
}


bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    return false;
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



void Platform::walk_filesystem(
    Function<8 * sizeof(void*), void(const char* path)> callback)
{
    // TODO...
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
// Platform
////////////////////////////////////////////////////////////////////////////////


static const std::unordered_map<std::string, sf::Keyboard::Key> key_lookup{
    {"Esc", sf::Keyboard::Escape},
    {"Up", sf::Keyboard::Up},
    {"Down", sf::Keyboard::Down},
    {"Left", sf::Keyboard::Left},
    {"Right", sf::Keyboard::Right},
    {"Return", sf::Keyboard::Return},
    {"A", sf::Keyboard::A},
    {"B", sf::Keyboard::B},
    {"C", sf::Keyboard::C},
    {"D", sf::Keyboard::D},
    {"E", sf::Keyboard::E},
    {"F", sf::Keyboard::F},
    {"G", sf::Keyboard::G},
    {"H", sf::Keyboard::H},
    {"I", sf::Keyboard::I},
    {"J", sf::Keyboard::J},
    {"K", sf::Keyboard::K},
    {"L", sf::Keyboard::L},
    {"M", sf::Keyboard::M},
    {"N", sf::Keyboard::N},
    {"O", sf::Keyboard::O},
    {"P", sf::Keyboard::P},
    {"Q", sf::Keyboard::Q},
    {"R", sf::Keyboard::R},
    {"S", sf::Keyboard::S},
    {"T", sf::Keyboard::T},
    {"U", sf::Keyboard::U},
    {"V", sf::Keyboard::V},
    {"W", sf::Keyboard::W},
    {"X", sf::Keyboard::X},
    {"Y", sf::Keyboard::Y},
    {"Z", sf::Keyboard::Z},
    {"Backspace", sf::Keyboard::Backspace}};


Platform::~Platform()
{
    delete data_;
}


// EWRAM is large, but has a narrower bus. The platform offers a window into
// EWRAM, called scratch space, for non-essential stuff. Right now, I am setting
// the buffer to ~100K in size. One could theoretically make the buffer almost
// 256kB, because I am using none of EWRAM as far as I know...
static ObjectPool<PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>,
                  scratch_buffer_count>
    scratch_buffer_pool("scratch-buffers");



static Optional<DateTime> start_time;


Optional<DateTime> Platform::SystemClock::initial_time()
{
    // return ::start_time;
    return std::nullopt;
}


Platform::Platform()
{
    ::__platform__ = this;

    // push_task(&::watchdog_task);

    data_ = new Data;
    if (not data_) {
        error("Failed to allocate context");
        exit(EXIT_FAILURE);
    }


    ::start_time = system_clock_.now();


    screen_.view_.set_size(screen_.size().cast<Float>());


    auto shader_folder = resource_path() + ("shaders" PATH_DELIMITER);
    // lisp::loadv<lisp::Symbol>("shader-dir").name_;


    if (not data_->color_shader_.loadFromFile(
            shader_folder + std::string("colorShader.frag"),
            sf::Shader::Fragment)) {
        error("Failed to load shader");
    }
    data_->color_shader_.setUniform("texture", sf::Shader::CurrentTexture);

    data_->fade_overlay_.setSize(
        {Float(screen_.size().x), Float(screen_.size().y)});


    data_->map_0_rt_.create(16 * 32, 16 * 16);
    data_->map_1_rt_.create(16 * 32, 16 * 16);
    data_->background_rt_.create(32 * 8, 32 * 8);

    keymap[(int)Key::left] = sf::Keyboard::Left;
    keymap[(int)Key::right] = sf::Keyboard::Right;
    keymap[(int)Key::up] = sf::Keyboard::Up;
    keymap[(int)Key::down] = sf::Keyboard::Down;
    keymap[(int)Key::action_1] = sf::Keyboard::X;
    keymap[(int)Key::action_2] = sf::Keyboard::Z;
    keymap[(int)Key::alt_1] = sf::Keyboard::A;
    keymap[(int)Key::alt_2] = sf::Keyboard::S;
    keymap[(int)Key::start] = sf::Keyboard::Return;
    keymap[(int)Key::select] = sf::Keyboard::Backspace;
}



static const char* const save_file_name = "save.dat";



void Platform::erase_save_sector()
{
    memset(save_buffer, 0xff, sizeof save_buffer);
}



bool Platform::write_save_data(const void* data, u32 length, u32 offset)
{
    memcpy(save_buffer + offset, data, length);

    std::ofstream out(save_file_name,
                      std::ios_base::out | std::ios_base::binary);

    out.write((const char*)save_buffer, ::save_capacity);

    return true;
}



bool Platform::read_save_data(void* buffer, u32 data_length, u32 offset)
{
    memcpy(buffer, save_buffer + offset, data_length);

    std::ifstream in(save_file_name, std::ios_base::in | std::ios_base::binary);

    if (!in) {
        return true;
    }

    in.read((char*)save_buffer, ::save_capacity);

    return true;
}



bool Platform::is_running() const
{
    return data()->window_.isOpen();
}



void Platform::sleep(u32 frames)
{
    const auto amount =
        frames * (std::chrono::duration_cast<std::chrono::microseconds>(
                      std::chrono::seconds(1)) /
                  60);

    std::this_thread::sleep_for(amount);

    sleep_time += amount.count();
}



void Platform::on_unrecoverrable_error(UnrecoverrableErrorCallback callback)
{
    // ...
}



void Platform::load_sprite_texture(const char* name)
{
    // std::lock_guard<std::mutex> guard(texture_swap_mutex);
    texture_swap_requests.push({TextureSwap::spritesheet, name});
}



void Platform::load_background_texture(const char* name)
{
    // ...
}



void Platform::load_tile0_texture(const char* name)
{
    sf::Image image;

    auto image_folder = resource_path() + ("images" PATH_DELIMITER);

    if (not image.loadFromFile(image_folder + name + ".png")) {
        error((std::string("failed to load texture ") + name).c_str());
        exit(EXIT_FAILURE);
    } else {
        info((std::string("loaded image ") + name).c_str());
    }
    image.createMaskFromColor({255, 0, 255, 255});

    PLATFORM.data()->current_tile0_image_.create(image.getSize().x,
                                                 image.getSize().y);
    PLATFORM.data()->current_tile0_image_.copy(
        image, 0, 0, {0, 0, (int)image.getSize().x, (int)image.getSize().y});

    if (image.getSize().x > 4032) {
        sf::Image replacement;
        replacement.create(4032, image.getSize().y);
        replacement.copy(image, 0, 0, {0, 0, 4032, (int)image.getSize().y});
        std::swap(image, replacement);
    }
    if (not PLATFORM.data()->tile0_texture_.loadFromImage(image)) {
        error("Failed to create texture");
        exit(EXIT_FAILURE);
    }

    PLATFORM.data()->map_0_[0].set_tilesize(image.getSize().y);

    PLATFORM.data()->map_0_changed_ = true;
}



void Platform::load_tile1_texture(const char* name)
{
    sf::Image image;

    auto image_folder = resource_path() + ("images" PATH_DELIMITER);

    if (not image.loadFromFile(image_folder + name + ".png")) {
        error((std::string("failed to load texture ") + name).c_str());
        exit(EXIT_FAILURE);
    } else {
        info((std::string("loaded image ") + name).c_str());
    }
    image.createMaskFromColor({255, 0, 255, 255});

    PLATFORM.data()->current_tile1_image_.create(image.getSize().x,
                                                 image.getSize().y);
    PLATFORM.data()->current_tile1_image_.copy(
        image, 0, 0, {0, 0, (int)image.getSize().x, (int)image.getSize().y});

    PLATFORM.data()->map_1_[0].set_tilesize(image.getSize().y);

    if (image.getSize().x > 4032) {
        sf::Image replacement;
        replacement.create(4032, image.getSize().y);
        replacement.copy(image, 0, 0, {0, 0, 4032, (int)image.getSize().y});
        std::swap(image, replacement);
    }
    if (not PLATFORM.data()->tile1_texture_.loadFromImage(image)) {
        error("Failed to create texture");
        exit(EXIT_FAILURE);
    }
    PLATFORM.data()->map_1_changed_ = true;
}



bool Platform::load_overlay_texture(const char* name)
{
    auto image_folder = resource_path() + ("images" PATH_DELIMITER);

    std::ifstream f(image_folder + name + ".png");
    if (not f.good()) {
        error(format("image % not found!", name));
        return false;
    }

    {
        // std::lock_guard<std::mutex> guard(texture_swap_mutex);
        texture_swap_requests.push({TextureSwap::overlay, name});
    }
    {
        // std::lock_guard<std::mutex> guard(glyph_requests_mutex);
        while (not glyph_requests.empty())
            glyph_requests.pop();
    }
    PLATFORM.data()->glyph_table_.clear();
    PLATFORM.data()->next_glyph_ = glyph_region_start;

    return true;
}



std::map<Layer, std::map<std::pair<u16, u16>, TileDesc>> tile_layers_;



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        TileDesc val,
                        Optional<u16> palette)
{
    tile_layers_[layer][{x, y}] = val;

    switch (layer) {
    case Layer::overlay:
        PLATFORM.data()->overlay_.set_tile(x, y, val);
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
        PLATFORM.data()->map_0_changed_ = true;
        PLATFORM.data()->map_0_[0].set_tile(x, y, val);
        break;

    case Layer::map_1_ext:
    case Layer::map_1:
        PLATFORM.data()->map_1_changed_ = true;
        PLATFORM.data()->map_1_[0].set_tile(x, y, val);
        break;

    case Layer::background:
        PLATFORM.data()->background_changed_ = true;
        PLATFORM.data()->background_.set_tile(x, y, val);
        break;
    }
}


void Platform::set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors)
{
    // FIXME: implement custom text colors!

    set_tile(Layer::overlay, x, y, glyph);
}


TileDesc Platform::get_tile(Layer layer, u16 x, u16 y)
{
    return tile_layers_[layer][{x, y}];
}


void Platform::fill_overlay(u16 tile_desc)
{
    for (auto& kvp : tile_layers_[Layer::overlay]) {
        kvp.second = tile_desc;
    }

    // std::lock_guard<std::mutex> guard(::tile_swap_mutex);

    for (int i = 0; i < PLATFORM.data()->overlay_.size().x; ++i) {
        for (int j = 0; j < PLATFORM.data()->overlay_.size().y; ++j) {
            PLATFORM.data()->overlay_.set_tile(i, j, tile_desc);
        }
    }
}


void Platform::set_overlay_origin(Float x, Float y)
{
    PLATFORM.data()->overlay_origin_ = {x, y};
}


static bool glyph_mode;


void Platform::enable_glyph_mode(bool enabled)
{
    glyph_mode = true;
}


TileDesc Platform::map_glyph(const utf8::Codepoint& glyph,
                             const TextureMapping& mapping)
{
    if (not glyph_mode) {
        return 495;
    }
    auto& glyphs = PLATFORM.data()->glyph_table_;

    auto found = glyphs.find(mapping.offset_);
    if (found not_eq glyphs.end()) {
        return found->second;
    } else {
        const auto loc = PLATFORM.data()->next_glyph_++;
        glyphs[mapping.offset_] = loc;

        {
            // This code is so wasteful... so many intermediary images... FIXME.

            auto& texture = PLATFORM.data()->overlay_texture_;
            auto old_texture_img = texture.copyToImage();

            sf::Image new_texture_image;
            new_texture_image.create((loc + 1) * 8,
                                     old_texture_img.getSize().y);

            new_texture_image.copy(old_texture_img,
                                   0,
                                   0,
                                   {0,
                                    0,
                                    (int)old_texture_img.getSize().x,
                                    (int)old_texture_img.getSize().y});

            new_texture_image.copy(data_->character_source_image_,
                                   loc * 8,
                                   0,
                                   {mapping.offset_ * 8, 0, 8, 8},
                                   true);

            const auto glyph_background_color =
                data_->character_source_image_.getPixel(0, 0);

            const auto font_fg_color = new_texture_image.getPixel(648, 0);
            const auto font_bg_color = new_texture_image.getPixel(649, 0);

            for (int x = 0; x < 8; ++x) {
                for (int y = 0; y < 8; ++y) {
                    const auto px = new_texture_image.getPixel(loc * 8 + x, y);
                    if (px == glyph_background_color) {
                        new_texture_image.setPixel(
                            loc * 8 + x, y, font_bg_color);
                    } else {
                        new_texture_image.setPixel(
                            loc * 8 + x, y, font_fg_color);
                    }
                }
            }

            texture.loadFromImage(new_texture_image);
        }

        return loc;
    }
}


void Platform::fatal(const char* msg)
{
    exit(1);
}



u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    return 0;
}


void Platform::set_palette(Layer layer, u16 x, u16 y, u16 palette)
{
}



static std::map<std::string, std::string> files;


std::pair<const char*, u32> Platform::load_file(const char* folder,
                                                const char* filename) const
{
    const auto name = std::string(folder) + PATH_DELIMITER + filename;
    auto found = files.find(name);
    if (found == files.end()) {
        std::fstream file(resource_path() + name);
        std::stringstream buffer;
        buffer << file.rdbuf();
        files[name] = buffer.str();
    } else {
        return {found->second.c_str(), found->second.length()};
    }

    found = files.find(name);
    return {found->second.c_str(), found->second.length()};
}


void start(Platform&);


int argc = 0;
char** argv = nullptr;


int main(int argc, char** argv)
{
    rng::critical_state = time(nullptr);

    ::argc = argc;
    ::argv = argv;

    std::cout << "creating platform..." << std::endl;

    Platform pf;
    start(pf);
}



////////////////////////////////////////////////////////////////////////////////
// NetworkPeer
////////////////////////////////////////////////////////////////////////////////


struct NetworkPeerImpl
{
    sf::TcpSocket socket_;
    sf::TcpListener listener_;
    bool is_host_ = false;
    int poll_consume_position_ = 0;
};


Platform::NetworkPeer::NetworkPeer() : impl_(nullptr)
{
    auto impl = new NetworkPeerImpl;

    impl->socket_.setBlocking(true);

    impl_ = impl;
}


void Platform::NetworkPeer::disconnect()
{
    auto impl = (NetworkPeerImpl*)impl_;
    impl->socket_.disconnect();

    if (is_connected()) {
        error("disconnect failed?!");
    }
}


bool Platform::NetworkPeer::is_host() const
{
    return ((NetworkPeerImpl*)impl_)->is_host_;
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

    auto impl = (NetworkPeerImpl*)impl_;
    return impl->socket_.getRemoteAddress() not_eq sf::IpAddress::None;
}


bool Platform::NetworkPeer::send_message(const Message& message)
{
    return true; // TODO

    auto impl = (NetworkPeerImpl*)impl_;

    std::size_t sent = 0;
    impl->socket_.send(message.data_, message.length_, sent);

    if (sent not_eq message.length_) {
        warning("part of message not sent!");
        return false;
    }

    return true;
}


static std::vector<u8> receive_buffer;


void Platform::NetworkPeer::update()
{
    auto impl = (NetworkPeerImpl*)impl_;

    if (impl->poll_consume_position_) {
        receive_buffer.erase(receive_buffer.begin(),
                             receive_buffer.begin() +
                                 impl->poll_consume_position_);
    }

    impl->poll_consume_position_ = 0;

    std::array<char, 1024> buffer = {0};
    std::size_t received = 0;

    while (true) {
        impl->socket_.receive(buffer.data(), sizeof buffer, received);

        if (received > 0) {
            std::copy(buffer.begin(),
                      buffer.begin() + received,
                      std::back_inserter(receive_buffer));
        }

        if (received < sizeof buffer) {
            break;
        }
    }
}


Optional<Platform::NetworkPeer::Message> Platform::NetworkPeer::poll_message()
{
    auto impl = (NetworkPeerImpl*)impl_;

    if (receive_buffer.empty()) {
        return {};
    }

    if (impl->poll_consume_position_ >= (int)receive_buffer.size()) {
        return {};
    }

    return Message{(u8*)&receive_buffer[impl->poll_consume_position_],
                   (u32)receive_buffer.size() - impl->poll_consume_position_};
}


void Platform::NetworkPeer::poll_consume(u32 length)
{
    auto impl = (NetworkPeerImpl*)impl_;
    impl->poll_consume_position_ += length;
}


Platform::NetworkPeer::~NetworkPeer()
{
    delete (NetworkPeerImpl*)impl_;
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
// SystemClock
////////////////////////////////////////////////////////////////////////////////


Optional<DateTime> Platform::SystemClock::now()
{
    const auto t = std::chrono::system_clock::now();

    auto tt = std::chrono::system_clock::to_time_t(t);

    tm local_tm = *localtime(&tt);

    DateTime info;
    info.date_.year_ = local_tm.tm_year;
    info.date_.month_ = local_tm.tm_mon;
    info.date_.day_ = local_tm.tm_mday;
    info.hour_ = local_tm.tm_hour;
    info.minute_ = local_tm.tm_min;
    info.second_ = local_tm.tm_sec;

    return info;
}



void Platform::SystemClock::configure(DateTime dt)
{
}



void Platform::SystemClock::init()
{
}


Platform::SystemClock::SystemClock()
{
}



////////////////////////////////////////////////////////////////////////////////
//
// misc
//
////////////////////////////////////////////////////////////////////////////////



void Platform::blit_t0_erase(u16 index)
{
    PLATFORM.data()->map_0_changed_ = true;
    int y = index / 30;
    int x = index % 30;
    PLATFORM.data()->depth_0_[x][y] = 0;
    for (int i = 0; i < 6; ++i) {
        PLATFORM.data()->map_0_[i].set_tile(x, y, 0);
    }
}



void Platform::blit_t1_erase(u16 index)
{
    PLATFORM.data()->map_1_changed_ = true;
    int y = index / 30;
    int x = index % 30;
    PLATFORM.data()->depth_1_[x][y] = 0;
    for (int i = 0; i < 6; ++i) {
        PLATFORM.data()->map_1_[i].set_tile(x, y, 0);
    }
}



void Platform::blit_t0_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
    return; // FIXME
    PLATFORM.data()->map_0_changed_ = true;
    int y = to_index / 30;
    int x = to_index % 30;
    if (hard) {
        blit_t0_erase(to_index);
    }
    int& d = PLATFORM.data()->depth_0_[x][y];
    if (d >= 6) {
        return;
    }
    PLATFORM.data()->map_0_[d++].set_tile(x, y, from_index);
}



void Platform::blit_t1_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
    return; // FIXME
    PLATFORM.data()->map_1_changed_ = true;
    int y = to_index / 30;
    int x = to_index % 30;
    if (hard) {
        blit_t1_erase(to_index);
    }
    int& d = PLATFORM.data()->depth_1_[x][y];
    if (d >= 6) {
        return;
    }
    PLATFORM.data()->map_1_[d++].set_tile(x, y, from_index);
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



void Platform::overwrite_overlay_tile(u16 index, const EncodedTile& t)
{
}



static const int tile_reserved_count = 8;
static const int tile_mapping_slots = 111 - tile_reserved_count;

using TileMappings = s16[tile_mapping_slots];

static TileMappings tile0_mappings = {0};
static TileMappings tile1_mappings = {0};



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



static TileDesc map_tile_chunk(TileMappings mappings,
                               TileDesc src,
                               sf::Image& src_img,
                               sf::Texture& dest)
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

    auto texture_img = dest.copyToImage();
    texture_img.copy(
        src_img, i * 16, 0, {(tile_data_start + src) * 16, 0, 16, 16});

    dest.loadFromImage(texture_img);

    return i;
}



TileDesc Platform::map_tile0_chunk(TileDesc src)
{
    data()->map_0_changed_ = true;
    return map_tile_chunk(tile0_mappings,
                          src,
                          data()->current_tile0_image_,
                          data()->tile0_texture_);
}



TileDesc Platform::map_tile1_chunk(TileDesc src)
{
    data()->map_1_changed_ = true;
    return map_tile_chunk(tile1_mappings,
                          src,
                          data()->current_tile1_image_,
                          data()->tile1_texture_);
}



Platform::TilePixels Platform::extract_tile(Layer layer, u16 tile)
{
    return {};
}



Platform::EncodedTile Platform::encode_tile(u8 tile_data[16][16])
{
    // TODO...
    return {};
}



Vec2<u16> Platform::get_scroll(Layer layer)
{
    switch (layer) {
    case Layer::background:
    case Layer::overlay:
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
        return {(u16)PLATFORM.data()->map_0_xscroll_,
                (u16)PLATFORM.data()->map_0_yscroll_};

    case Layer::map_1_ext:
    case Layer::map_1:
        return {(u16)PLATFORM.data()->map_1_xscroll_,
                (u16)PLATFORM.data()->map_1_yscroll_};
    }

    return {};
}



void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
    s16 sx = x;
    s16 sy = y;

    sx %= 512;

    switch (layer) {
    case Layer::overlay:
        // TODO...
        break;

    case Layer::map_0_ext:
        PLATFORM.data()->map_0_xscroll_ = sx;
        PLATFORM.data()->map_0_yscroll_ = sy;
        break;

    case Layer::map_1_ext:
        PLATFORM.data()->map_1_xscroll_ = sx;
        PLATFORM.data()->map_1_yscroll_ = sy;
        break;

    case Layer::map_0:
        PLATFORM.data()->map_0_xscroll_ = sx;
        PLATFORM.data()->map_0_yscroll_ = sy;
        break;

    case Layer::map_1:
        PLATFORM.data()->map_1_xscroll_ = sx;
        PLATFORM.data()->map_1_yscroll_ = sy;
        break;

    case Layer::background:
        // TODO...
        break;
    }
}



void Platform::load_overlay_chunk(TileDesc dst,
                                  TileDesc src,
                                  u16 count,
                                  const char* image_file)
{
    auto& texture = data()->overlay_texture_;
    auto texture_img = texture.copyToImage();

    texture_img.copy(
        data()->current_overlay_image_, dst * 8, 0, {src * 8, 0, count * 8, 8});

    texture.loadFromImage(texture_img);
}



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


void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
    tile_layers_[layer][{x, y}] = val;

    switch (layer) {
    case Layer::overlay:
        PLATFORM.data()->overlay_.set_tile(x, y, val);
        break;

    case Layer::map_0_ext:
    case Layer::map_1_ext:
        break;

    case Layer::map_0:
        PLATFORM.data()->map_0_changed_ = true;
        PLATFORM.data()->map_0_[0].set_tile(x, y, val);
        // std::cout << "set tile " << x << ", " << y << ", " << val << std::endl;
        break;

    case Layer::map_1:
        PLATFORM.data()->map_1_changed_ = true;
        PLATFORM.data()->map_1_[0].set_tile(x, y, val);
        break;

    case Layer::background:
        PLATFORM.data()->background_changed_ = true;
        PLATFORM.data()->background_.set_tile(x, y, val);
        break;
    }
}



void Platform::Screen::set_shader(Shader shader)
{
    // ...
}



void Platform::Screen::set_shader_argument(int arg)
{
}



void Platform::override_priority(Layer layer, int priority)
{
    // TODO...
}



#ifdef _WIN32
// I am not interested in pulling in all of the stuff in windows.h, some of which is truely horrendous code.A
#ifndef _WINDEF_
class HINSTANCE__;
typedef HINSTANCE__* HINSTANCE;
typedef char* LPSTR;
#endif
int WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    return main();
}
#endif
