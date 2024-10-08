////////////////////////////////////////////////////////////////////////////////
//
// Copyright 2024 Evan Bowman
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the “Software”), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.  THE SOFTWARE IS PROVIDED
// “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
// LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.//
//
////////////////////////////////////////////////////////////////////////////////


#include "number/random.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include <SDL2/SDL.h>
#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <sstream>
#include <thread>



#ifdef _WIN32
#define PATH_DELIMITER "\\"
#else
#define PATH_DELIMITER "/"
#endif



std::string resource_path();



void start(Platform&);



SDL_Window* window = nullptr;
SDL_Surface* screenSurface = nullptr;
bool sdl_running = true;



Platform* __platform__ = nullptr;



int main(int argc, char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    window = SDL_CreateWindow("Skyland",
                              SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED,
                              480,
                              320,
                              0);

    if (window == NULL) {
        fprintf(
            stderr, "SDL window failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    screenSurface = SDL_GetWindowSurface(window);

    SDL_GL_SetSwapInterval(1); // This enables vsync for GL backends.x


    rng::critical_state = time(nullptr);

    Platform pf;

    __platform__ = &pf;
    start(pf);

    SDL_DestroyWindow(window);
    SDL_Quit();
}



////////////////////////////////////////////////////////////////////////////////
// Misc Platform stuff...
////////////////////////////////////////////////////////////////////////////////


void Platform::restart()
{
    // ...
    while (1)
        ;
}



std::map<Layer, std::map<std::pair<u16, u16>, TileDesc>> tile_layers_;



TileDesc Platform::get_tile(Layer layer, u16 x, u16 y)
{
    return tile_layers_[layer][{x, y}];
}



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        TileDesc val,
                        Optional<u16> palette)
{
    tile_layers_[layer][{x, y}] = val;

    // TODO...
    switch (layer) {
    case Layer::overlay:
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
        break;

    case Layer::map_1_ext:
    case Layer::map_1:
        break;

    case Layer::background:
        break;
    }
}



void Platform::set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors)
{
    // FIXME: implement custom text colors!

    set_tile(Layer::overlay, x, y, glyph);
}



void Platform::Keyboard::rumble(bool enabled)
{
}



void Platform::Keyboard::poll()
{
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        if (e.type == SDL_QUIT) {
            sdl_running = false;
        }
    }
}



static std::map<std::string, std::string> files;



std::pair<const char*, u32> Platform::load_file(const char* folder,
                                                const char* filename) const
{
    const auto name = std::string(folder) + PATH_DELIMITER + filename;

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
            error(format("missing file %", (resource_path() + path).c_str()));
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



void Platform::DynamicTexture::remap(u16 spritesheet_offset)
{
    // No need to do anything on the desktop platform. All of our texture fits
    // in vram, so we do not need to manage any "virtual" tile indices.
}



Optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    return std::nullopt;
}



static const char* const save_file_name = "save.dat";



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



Vec2<u16> Platform::get_scroll(Layer layer)
{
    return {};
}



void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
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



Platform& Platform::instance()
{
    return *__platform__;
}



void* Platform::system_call(const char* feature_name, void* arg)
{
    return nullptr;
}



u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    return 0;
}



void Platform::set_palette(Layer layer, u16 x, u16 y, u16 palette)
{
}



void Platform::fill_overlay(u16 tile_desc)
{
}



void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
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



TileDesc Platform::map_tile0_chunk(TileDesc src)
{
    return 0;
}



TileDesc Platform::map_tile1_chunk(TileDesc src)
{
    return 0;
}



void Platform::walk_filesystem(
    Function<8 * sizeof(void*), void(const char* path)> callback)
{
    // TODO...
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
    return 495; // todo...
}



void Platform::override_priority(Layer layer, int priority)
{
    // TODO...
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



void Platform::load_overlay_chunk(TileDesc dst,
                                  TileDesc src,
                                  u16 count,
                                  const char* image_file)
{
}



void Platform::load_tile0_texture(const char* name)
{
}



void Platform::load_tile1_texture(const char* name)
{
}



void Platform::set_overlay_origin(Float x, Float y)
{
}



void Platform::load_sprite_texture(const char* name)
{
}



void Platform::load_background_texture(const char* name)
{
}



void Platform::clear_tile0_mappings()
{
}



void Platform::clear_tile1_mappings()
{
}



void Platform::fatal(const char* msg)
{
    std::cerr << msg << std::endl;
    exit(1);
}



bool Platform::load_overlay_texture(const char* name)
{
    return true;
}



void Platform::on_unrecoverrable_error(UnrecoverrableErrorCallback callback)
{
    // ...
}



void Platform::sleep(u32 frames)
{
    const auto amount =
        frames * (std::chrono::duration_cast<std::chrono::microseconds>(
                      std::chrono::seconds(1)) /
                  60);

    std::this_thread::sleep_for(amount);
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
    namespace chrono = std::chrono;

    auto clk = chrono::high_resolution_clock::now();
    auto elapsed = chrono::duration_cast<chrono::microseconds>(clk - clk_prev);

    clk_prev = clk;

    return elapsed.count();
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



Optional<DateTime> Platform::SystemClock::initial_time()
{
    // return ::start_time;
    return std::nullopt;
}



////////////////////////////////////////////////////////////////////////////////
// RemoteConsole
////////////////////////////////////////////////////////////////////////////////



std::mutex console_rcv_mutex;
std::string console_rcv_string;

std::mutex console_send_mutex;
std::queue<std::string> console_send_queue;



void console_thread_main()
{
    while (sdl_running) {
        std::string text;

        if (std::getline(std::cin, text)) {
            std::lock_guard<std::mutex> lock(console_rcv_mutex);
            std::swap(console_rcv_string, text);
        }
    }
}



void Platform::RemoteConsole::start()
{
    std::thread console_thread(console_thread_main);
    console_thread.detach();
}



auto Platform::RemoteConsole::readline() -> Optional<Line>
{
    std::lock_guard<std::mutex> lock(console_rcv_mutex);
    if (not console_rcv_string.empty()) {
        Line ret = console_rcv_string.c_str();
        console_rcv_string.clear();
        return ret;
    }

    return {};
}



bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    std::cout << text << "\n" << prompt << std::flush;
    return false;
}



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
}



void Platform::Screen::set_shader_argument(int arg)
{
}



void Platform::Screen::draw_batch(TextureIndex t,
                                  const Buffer<Vec2<s32>, 64>& coords,
                                  const SpriteBatchOptions& opts)
{
}



void Platform::Screen::draw(const Sprite& spr)
{
}



void Platform::Screen::clear()
{
    SDL_FillRect(screenSurface,
                 nullptr,
                 SDL_MapRGB(screenSurface->format, 0xFF, 0xFF, 0xFF));
}



void Platform::Screen::display()
{
    SDL_UpdateWindowSurface(window);
}



const Platform::Screen::Touch* Platform::Screen::touch() const
{
    return nullptr;
}



Vec2<u32> Platform::Screen::size() const
{
    return {240, 160};
}



static u8 last_fade_amt;



void Platform::Screen::fade(float amount,
                            ColorConstant k,
                            Optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{
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



void Platform::Speaker::play_music(const char* name, Microseconds offset)
{
}



Platform::Speaker::Speaker()
{
}



void Platform::Speaker::set_music_speed(MusicSpeed speed)
{
}



void Platform::Speaker::set_music_volume(u8 volume)
{
}



void Platform::Speaker::set_sounds_volume(u8 volume)
{
}



void Platform::Speaker::stash_sounds()
{
}



void Platform::Speaker::restore_sounds()
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



void Platform::Speaker::start()
{
}



Microseconds Platform::Speaker::track_length(const char* name)
{
    return 1;
}



bool Platform::Speaker::is_sound_playing(const char* name)
{
    return false;
}



bool Platform::Speaker::is_music_playing(const char* name)
{
    return false;
}



const char* Platform::Speaker::completed_music()
{
    return nullptr;
}



Buffer<const char*, 4> Platform::Speaker::completed_sounds()
{
    return {};
}



void Platform::Speaker::stop_sound(const char* name)
{
}



void Platform::Speaker::clear_sounds()
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



void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   Optional<Vec2<Float>> position)
{
}



void Platform::Speaker::stop_music()
{
}



////////////////////////////////////////////////////////////////////////////////
// Platform impl
////////////////////////////////////////////////////////////////////////////////



Platform::Platform()
{
    std::ifstream in(save_file_name, std::ios_base::in | std::ios_base::binary);
    if (in) {
        in.read((char*)save_buffer, ::save_capacity);
    }
}



Platform::~Platform()
{
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
