///////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "dateTime.hpp"
#include "function.hpp"
#include "graphics/sprite.hpp"
#include "graphics/view.hpp"
#include "key.hpp"
#include "layer.hpp"
#include "memory/buffer.hpp"
#include "memory/rc.hpp"
#include "number/numeric.hpp"
#include "scratch_buffer.hpp"
#include "severity.hpp"
#include "sound.hpp"
#include "unicode.hpp"
#include <array>
#include <optional>


using TileDesc = u16;



struct FontColors
{
    ColorConstant foreground_;
    ColorConstant background_;
};


template <typename T> class Vector;


// Anything platform specific should be defined here.


////////////////////////////////////////////////////////////////////////////////
// Platform
////////////////////////////////////////////////////////////////////////////////


enum class ShaderPalette {
    tile0 = 0,
    overlay = 1,
    tile1 = 2,
    background = 11,
    spritesheet = 16,
};



#define PLATFORM_EXTENSION(NAME, ...)                                          \
    {                                                                          \
        if (PLATFORM.get_extensions().NAME) {                                  \
            PLATFORM.get_extensions().NAME(__VA_ARGS__);                       \
        }                                                                      \
    }



class Platform
{
public:
    class Screen;
    class Keyboard;
    class Logger;
    class Speaker;
    class NetworkPeer;
    class DeltaClock;
    class RemoteConsole;

    using DeviceName = StringBuffer<23>;
    DeviceName device_name() const;

    using ModelName = StringBuffer<24>;
    ModelName model_name() const;


    static Platform& instance();


    struct alignas(4) EncodedTile
    {
        u8 bytes_[128];
    };


    EncodedTile encode_tile(u8 tile_data[16][16]);


    void memset_words(void* dest, u8 byte, u32 word_count);


    // NOTE: overlay tiles are 8x8px, so only the top left quarter of the
    // encoded tile data will be used.
    void overwrite_overlay_tile(u16 index, const EncodedTile& t);

    void overwrite_t0_tile(u16 index, const EncodedTile& t);
    void overwrite_t1_tile(u16 index, const EncodedTile& t);

    void overwrite_sprite_tile(u16 index, const EncodedTile& t);


    // Intended to allow usage of the t0 tile layer as a sort of RenderTexture
    // (frame buffer). Writes a 8x8 px tile block specified by from_index to
    // a location specified by to_index.
    void blit_t0_tile_to_texture(u16 from_index,
                                 u16 to_index,
                                 // Completely overwrite existing contents
                                 // of to_index (faster).
                                 bool hard);

    void blit_t0_erase(u16 index);


    void blit_t1_tile_to_texture(u16 from_index,
                                 u16 to_index,
                                 // Completely overwrite existing contents
                                 // of to_index (faster).
                                 bool hard);

    void blit_t1_erase(u16 index);


    struct TilePixels
    {
        u8 data_[16][16];
    };


    TilePixels extract_tile(Layer layer, u16 tile);


    void override_priority(Layer layer, int priority);


    Screen& screen()
    {
        return screen_;
    }


    Keyboard& keyboard()
    {
        return keyboard_;
    }


    Logger& logger()
    {
        return logger_;
    }

    Speaker& speaker()
    {
        return speaker_;
    }

    NetworkPeer& network_peer()
    {
        return network_peer_;
    }

    RemoteConsole& remote_console()
    {
        return console_;
    }

    DeltaClock& delta_clock()
    {
        return delta_clock_;
    }

    // NOTE: I must admit, the platform class interface has become quite
    // bloated.

    // On some platforms, fatal() will trigger a soft reset. But soft-reset is
    // mostly reserved for bare-metal platforms, where it's usually easy to
    // perform a soft reset via a simple BIOS call to reset the ROM. When
    // running as a windowed application within an OS, though, it's not as
    // simple to reset a process back to its original state at time of creation,
    // so it's easier in those cases to just exit. In any event, fatal() does
    // not return.
    [[noreturn]] static void fatal(const char* message);

    template <u32 size> static void fatal(const StringBuffer<size>& str)
    {
        fatal(str.c_str());
    }


    [[noreturn]] static void restart();


    struct TextureMapping
    {
        const char* texture_name_;
        u16 offset_;
    };

    // Supplied with a unicode codepoint, this function should provide an offset
    // into a texture image from which to load a glyph image.
    using TextureCpMapper =
        Optional<TextureMapping> (*)(const utf8::Codepoint&);

    // Map a glyph into the vram space reserved for the overlay tile layer.
    TileDesc map_glyph(const utf8::Codepoint& glyph,
                       const TextureMapping& mapping);


    // Copy a chunk of the overlay texture into another chunk of the
    // texture. Allows you to create textures larger than will fit in vram, and
    // swap in sections.
    void load_overlay_chunk(TileDesc dst,
                            TileDesc src,
                            u16 count,
                            const char* image_file = nullptr);

    TileDesc map_tile0_chunk(TileDesc tile);
    TileDesc map_tile1_chunk(TileDesc tile);

    void clear_tile0_mappings();
    void clear_tile1_mappings();


    static constexpr const int dynamic_texture_count = 6;

    // While spritesheets are generally fixed, the platform provides a number of
    // flexible texture indices, allowing memory to be mapped dynamically into
    // video ram. By calling DynamicTexture::remap(), the platform will map the
    // requested texture index into video memory, and the index may then be used
    // when drawing sprites.
    class DynamicTexture
    {
    public:
        // This constructor is not private, but still, do not create
        // DynamicTexture instances except by calling
        // Platform::make_dynamic_texture();
        DynamicTexture(u8 mapping_index) : mapping_index_(mapping_index)
        {
        }

        DynamicTexture(DynamicTexture&) = delete;

        void remap(u16 spritesheet_offset);

        u8 mapping_index() const
        {
            return mapping_index_;
        }

    private:
        u8 mapping_index_;
    };

    using DynamicTexturePtr =
        Rc<DynamicTexture,
           PooledRcControlBlock<DynamicTexture, dynamic_texture_count>>;


    Optional<DynamicTexturePtr> make_dynamic_texture();


    // In glyph mode, the platform will automatically unmap glyphs when their
    // tiles are overwritten by set_tile.
    void enable_glyph_mode(bool enabled);


    // NOTE: For the overlay and background, the tile layers consist of 32x32
    // tiles, where each tiles is 8x8 pixels. The overlay and the background
    // wrap when scrolled. Map tiles, on the other hand, are 32x24 pixels, and
    // the whole map consists of 64x64 8x8 pixel tiles.
    void set_tile(Layer layer,
                  u16 x,
                  u16 y,
                  TileDesc val,
                  Optional<u16> palette = {});


    void set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val);


    void set_palette(Layer layer, u16 x, u16 y, u16 palette);
    u16 get_palette(Layer layer, u16 x, u16 y);

    void set_flip(Layer layer, u16 x, u16 y, bool xflip, bool yflip);


    void set_scroll(Layer layer, u16 x, u16 y);
    Vec2<u16> get_scroll(Layer layer);


    // A special version of set_tile meant for glyphs. Allows you to set custom
    // colors. If the platform runs out of room for colors, the oldest ones will
    // be overwritten.
    //
    // NOTE: Custom colored fonts are currently incompatible with screen
    // fades. Custom colored text will not be faded.
    void set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors);

    // This function is not necessarily implemented efficiently, may in fact be
    // very slow.
    TileDesc get_tile(Layer layer, u16 x, u16 y);


    void fill_overlay(u16 TileDesc);

    void set_overlay_origin(Float x, Float y);


    void load_sprite_texture(const char* name);
    void load_tile0_texture(const char* name);
    void load_tile1_texture(const char* name);
    bool load_overlay_texture(const char* name);
    void load_background_texture(const char* name);


    // Sleep halts the game for an amount of time equal to some number
    // of game updates. Given that the game should be running at
    // 60fps, one update equals 1/60 of a second.
    using Frame = u32;
    void sleep(Frame frames);


    bool is_running() const;


    using UnrecoverrableErrorCallback =
        Function<4 * sizeof(void*), void(const char*)>;
    void on_unrecoverrable_error(UnrecoverrableErrorCallback callback);

    using TaskPointer = void (*)();
    using OldTask = TaskPointer;
    using NewTask = TaskPointer;

    OldTask set_background_task(NewTask task);


    bool write_save_data(const void* data, u32 length, u32 offset);
    bool read_save_data(void* buffer, u32 data_length, u32 offset);

    int save_capacity();

    void erase_save_sector();



    // For historical reasons, allows you to specify a folder and filename
    // separately. If you do not care for this behavior, supply an empty string
    // in the folder argument, and the path in the filename argument.
    std::pair<const char*, u32> load_file(const char* folder,
                                          const char* filename) const;


    const char* load_file_contents(const char* folder,
                                   const char* filename) const
    {
        return load_file(folder, filename).first;
    }



    void walk_filesystem(Function<8 * sizeof(void*), void(const char* path)>);



    ////////////////////////////////////////////////////////////////////////////
    // DeltaClock
    ////////////////////////////////////////////////////////////////////////////


    class DeltaClock
    {
    public:
        ~DeltaClock();

        Microseconds reset();


        Microseconds last_delta() const;


        using TimePoint = int;

        TimePoint sample() const;


        static Microseconds duration(TimePoint t1, TimePoint t2);


    private:
        friend class Platform;

        DeltaClock();

        void* impl_;
    };



    ////////////////////////////////////////////////////////////////////////////
    // Screen
    ////////////////////////////////////////////////////////////////////////////



    class Screen
    {
    public:
        static constexpr u32 sprite_limit = 128;


        void draw(const Sprite& spr);

        struct SpriteBatchOptions
        {
            SpriteBatchOptions()
                : position_absolute_(false), alpha_(Sprite::Alpha::opaque)
            {
            }

            bool position_absolute_;
            Sprite::Alpha alpha_;
            Sprite::Size sz_ = Sprite::Size::w16_h32;
        };


        // Optimized drawing routine for multiple copies of the same sprite.
        void draw_batch(TextureIndex texture,
                        const Buffer<Vec2<s32>, 64>& coords,
                        const SpriteBatchOptions& opts = SpriteBatchOptions());

        void clear();

        void display();

        Vec2<u32> size() const;

        using Shader =
            Function<4 * sizeof(void*),
                     ColorConstant(ShaderPalette, ColorConstant, int, int)>;

        void set_shader(Shader shader);

        void set_shader_argument(int value);

        void set_view(const View& view)
        {
            view_ = view;
        }

        const View& get_view() const
        {
            return view_;
        }

        // Blend color into sprite existing screen colors, unless a base color
        // is specified, in which case, computes the resulting color from the
        // base color blended with the color parameter.
        // DEPRECATED:
        // The fade function fades the screen immediately. Being able to do a
        // screen fade right away serves a useful purpose, but causes tearing
        // when used for smooth screen fades. You should use the other fade
        // function, I'm retiring this one.
        void fade(float amount,
                  ColorConstant color = ColorConstant::rich_black,
                  Optional<ColorConstant> base = {},
                  bool include_sprites = true,
                  bool include_overlay = false);


        void schedule_fade(Float amount,
                           ColorConstant color = ColorConstant::rich_black,
                           bool include_sprites = true,
                           bool include_overlay = false,
                           bool include_background = true,
                           bool include_tiles = true,
                           bool dodge = false);


        bool fade_active() const;


        void pixelate(u8 amount,
                      bool include_overlay = true,
                      bool include_background = true,
                      bool include_sprites = true);

    private:
        Screen();

        friend class Platform;

        View view_;
        void* userdata_;
    };


    ////////////////////////////////////////////////////////////////////////////
    // Keyboard
    ////////////////////////////////////////////////////////////////////////////


    class Keyboard
    {
    public:
        using KeyStates = std::array<bool, int(Key::count)>;


        void poll();

        void rumble(bool enabled);

        using RestoreState = Bitvector<KeyStates{}.size()>;

        template <Key... k> bool all_pressed() const
        {
            return (... and states_[int(k)]);
        }

        template <Key... k> bool any_pressed() const
        {
            return (... or states_[int(k)]);
        }

        template <Key k> bool pressed() const
        {
            return states_[int(k)];
        }

        bool pressed(Key k) const
        {
            return states_[int(k)];
        }

        bool down_transition(Key k) const
        {
            return states_[int(k)] and not prev_[int(k)];
        }

        template <Key... k> bool down_transition() const
        {
            return (... or down_transition_helper<k>());
        }

        bool up_transition(Key k) const
        {
            return not states_[int(k)] and prev_[int(k)];
        }

        template <Key k> bool up_transition() const
        {
            return not states_[int(k)] and prev_[int(k)];
        }

        RestoreState dump_state()
        {
            return states_;
        }

        void restore_state(const RestoreState& state)
        {
            // NOTE: we're assigning both the current and previous state to the
            // restored state. Otherwise, we could re-trigger a keypress that
            // already happened
            for (u32 i = 0; i < state.size(); ++i) {
                prev_[i] = state[i];
                states_[i] = state[i];
            }
        }

    private:
        template <Key k> bool down_transition_helper() const
        {
            return states_[int(k)] and not prev_[int(k)];
        }

        Keyboard()
        {
            for (int i = 0; i < int(Key::count); ++i) {
                states_[i] = false;
                prev_[i] = false;
            }
        }

        friend class Platform;

        KeyStates prev_;
        KeyStates states_;
    };


    ////////////////////////////////////////////////////////////////////////////
    // Logger
    ////////////////////////////////////////////////////////////////////////////


    class Logger
    {
    public:
        void log(Severity severity, const char* msg);

        void flush();

        void clear();

        Vector<char>* data();

        void set_threshold(Severity severity);

    private:
        Logger();

        friend class Platform;
    };


    ////////////////////////////////////////////////////////////////////////////
    // Speaker
    ////////////////////////////////////////////////////////////////////////////


    class Speaker
    {
    public:
        void start();

        enum Note : u8 {
            invalid,
            C,
            CIS,
            D,
            DIS,
            E,
            F,
            FIS,
            G,
            GIS,
            A,
            AIS,
            B,
            count,
        };


        enum class Channel {
            square_1,
            square_2,
            noise,
            wave,
            invalid,
        };


        struct NoteDesc
        {
            struct RegularNote
            {
                Platform::Speaker::Note note_ : 4;
                u8 octave_ : 4;
            };

            struct NoiseFrequency
            {
                u8 frequency_select_ : 7;
                u8 wide_mode_ : 1;
            };

            union
            {
                RegularNote regular_;
                NoiseFrequency noise_freq_;
            };
        };


        enum class Effect {
            none,
            vibrato,
            duty,
            envelope,
        };


        struct ChannelSettings
        {
            u8 length_ : 6;
            u8 duty_ : 2;
            u8 envelope_step_ : 3;
            u8 envelope_direction_ : 1;
            u8 volume_ : 4;
        };


        // NOTE: All music will loop. It's just more efficient to implement the
        // music such that all tracks are either looped or non-looping, and I
        // decided to make tracks loop. If you want music to stop when finished,
        // stop it yourself.
        void play_music(const char* name, Microseconds offset);
        void stop_music();

        bool stream_music(const char* filename, Microseconds offset);


        // By convention, volume ranges from zero to nineteen (twenty volume
        // levels). Maximum performance when volume is zero or nineteen.

        static const u8 music_volume_max = 19;

        void set_music_volume(u8 volume);
        void set_sounds_volume(u8 volume);

        enum class MusicSpeed {
            regular,
            reversed,
            reversed4x,
            reversed8x,
            doubled,
            halved,
            reversed_slow
        };

        void set_music_speed(MusicSpeed speed);


        bool is_music_playing(const char* name);

        // A platform's speaker may only have the resources to handle a limited
        // number of overlapping sounds. For such platforms, currently running
        // sounds with a lower priority will be evicted, to make room for
        // higher-priority sounds.
        //
        // If you pass in an optional position, platforms that support spatial
        // audio will attenuate the sound based on distance to the listener (the
        // camera center);
        void play_sound(const char* name,
                        int priority,
                        Optional<Vec2<Float>> position = {});
        bool is_sound_playing(const char* name);


        // stash_sounds() pushes the currently playing sounds into a stash and
        // clears the active sound buffer. restore_sounds() copies the data in
        // the stash into the active sounds list. Sometimes, when pausing the
        // game, we want sound effects to halt, and resume when we continue
        // playing.
        void stash_sounds();
        void restore_sounds();


        // Return a buffer containing any sounds that finished playing.
        Buffer<const char*, 4> completed_sounds();

        StringBuffer<48> current_music();

        void clear_sounds();
        void stop_sound(const char* name);

        Microseconds track_length(const char* sound_or_music_name);

    private:
        friend class Platform;

        Speaker();
    };


    ////////////////////////////////////////////////////////////////////////////
    // NetworkPeer
    ////////////////////////////////////////////////////////////////////////////


    class NetworkPeer
    {
    public:
        NetworkPeer();
        NetworkPeer(const NetworkPeer&) = delete;
        ~NetworkPeer();

        void connect(const char* peer_address);
        void listen();

        void disconnect();

        bool is_connected() const;
        bool is_host() const;

        struct Message
        {
            const u8* data_;
            u32 length_;
        };

        enum Interface {
            serial_cable,
            internet,
        };

        Interface interface() const;

        // NOTE: You cannot transmit messages larger than 12 bytes. On the
        // gameboy advance, 12 byte messages require at least six serial io
        // interrupts, along with a bunch of timer interrupts. It's just not
        // realistic to make the messages too much larger, if you want to
        // receive the data within a reasonable amount of time on all platforms.
        static const u32 max_message_size = 6;

        // IMPORTANT!!! Messages containing all zeroes are not guaranteed to be
        // received on some platforms, so you should have at least some high
        // bits in your message.
        bool send_message(const Message& message);
        int send_queue_capacity() const;
        int send_queue_size() const;
        bool send_queue_empty() const
        {
            return send_queue_size() == 0;
        }

        void update();

        // The result of poll-message will include the length of the available
        // data in the network-peer's buffer. If the space in the buffer is
        // insufficient to frame a message, exit polling, and do not call
        // poll_consume() until there's enough space to fill an entire message.
        Optional<Message> poll_message();
        void poll_consume(u32 length);

    private:
        void* impl_;
    };


    ////////////////////////////////////////////////////////////////////////////
    // RemoteConsole
    ////////////////////////////////////////////////////////////////////////////


    class RemoteConsole
    {
    public:
#if defined(__GBA__) or defined(__NDS__)
        using Line = StringBuffer<1956>;
#else
        using Line = StringBuffer<8000>;
#endif

        void start();


        Optional<Line> readline();

        Line* peek_buffer();

        bool printline(const char* text, const char* prompt = "> ");

        void printline_blocking(const char* text, const char* prompt = "> ")
        {
            while (not printline(text, prompt))
                ;
        }
    };


    class Data;

    Data* data()
    {
        return data_;
    }

    const Data* data() const
    {
        return data_;
    }

    Platform(const Platform&) = delete;
    ~Platform();


    // Use these with the PLATFORM_EXTENSION macro. Intended to make porting
    // easier, by allowing certain functions to be left undeclared until the
    // person working on the port has time to implement them fully. This allows
    // us to leave certain functions unimplemented entirely on some platforms,
    // not requiring us to stub out unused functions.
    struct Extensions
    {
        bool (*stack_check)();
        void (*palette_sync)();
        void (*feed_watchdog)();
        void (*update_parallax_r1)(u8 scroll);
        void (*update_parallax_r2)(u8 scroll);
        void (*update_parallax_macro)(int scroll);
        void (*enable_parallax_clouds)(bool on);
        void (*vertical_parallax_enable)(bool on);
        void (*force_vsync)();
        void (*overlay_circle_effect)(int radius, int x, int y);
        void (*iris_wipe_effect)(int radius, int x, int y);
        void (*hibernate)();
        void (*print_memory_diagnostics)();
        void (*console_write_buffer)(Vector<char>& input);
        void (*dlc_download)(Vector<char>& output);
        void (*watchdog_on)();
        void (*watchdog_off)();
        u32 (*get_stack_usage)();
        u32 (*get_stack_size)();
        void (*restart)();
        void (*apply_color_correction)(const char* table_name);
        void (*enable_translucence)(const Buffer<Layer, 4>& layers);

        void (*psg_play_note)(Speaker::Channel c, Speaker::NoteDesc note);
        void (*psg_stop_note)(Speaker::Channel c);
        void (*psg_apply_effect)(Speaker::Channel c,
                                 Speaker::Effect e,
                                 u8 arg,
                                 Microseconds delta);
        void (*psg_init_square_1)(Speaker::ChannelSettings s);
        void (*psg_init_square_2)(Speaker::ChannelSettings s);
        void (*psg_init_wave)(Speaker::ChannelSettings s);
        void (*psg_init_noise)(Speaker::ChannelSettings s);
        void (*rotate_palette)(Layer l, u8 range_start, u8 range_end);

        bool (*__test_compare_sound)(const char* sound_name);
    };


    const Extensions& get_extensions();


private:
    Platform();

    friend int main(int argc, char** argv);

    NetworkPeer network_peer_;
    DeltaClock delta_clock_;
    RemoteConsole console_;
    Screen screen_;
    Keyboard keyboard_;
    Speaker speaker_;
    Logger logger_;
    Data* data_ = nullptr;
};



// NOTE: technically, nothing platform-specific in these shader functions. They
// could be easily defined outside of the platform class. But a platform may
// know specific things about hardware color representation, allowing certain
// color effects to be computed more quickly in a platform-specific way. For
// example: one of the build targets, the Gameboy Advance, uses five-bit bgr,
// and there's no need to convert to a 32-bit color constant (eight bits per
// color) and back to five bit color again.
ColorConstant
passthrough_shader(ShaderPalette palette, ColorConstant k, int arg, int index);

ColorConstant
grayscale_shader(ShaderPalette palette, ColorConstant k, int arg, int index);

ColorConstant
contrast_shader(ShaderPalette palette, ColorConstant k, int arg, int index);


extern Platform* __platform__;
#define PLATFORM (*__platform__)


// Helper function for drawing background tiles larger than the default size (8x8 pixels)
inline void draw_image(TileDesc start_tile,
                       u16 start_x,
                       u16 start_y,
                       u16 width,
                       u16 height,
                       Layer layer)
{

    u16 tile = start_tile;

    for (u16 y = start_y; y < start_y + height; ++y) {
        for (u16 x = start_x; x < start_x + width; ++x) {
            PLATFORM.set_tile(layer, x, y, tile++);
        }
    }
}


template <Key k> bool key_down()
{
    return PLATFORM.keyboard().down_transition<k>();
}


inline void debug(const char* msg)
{
    PLATFORM.logger().log(Severity::debug, msg);
}
inline void info(const char* msg)
{
    PLATFORM.logger().log(Severity::info, msg);
}
inline void warning(const char* msg)
{
    PLATFORM.logger().log(Severity::warning, msg);
}
inline void error(const char* msg)
{
    PLATFORM.logger().log(Severity::error, msg);
}

template <u32 size> void debug(const StringBuffer<size>& buffer)
{
    PLATFORM.logger().log(Severity::debug, buffer.c_str());
}
template <u32 size> void info(const StringBuffer<size>& buffer)
{
    PLATFORM.logger().log(Severity::debug, buffer.c_str());
}
template <u32 size> void warning(const StringBuffer<size>& buffer)
{
    PLATFORM.logger().log(Severity::debug, buffer.c_str());
}
template <u32 size> void error(const StringBuffer<size>& buffer)
{
    PLATFORM.logger().log(Severity::debug, buffer.c_str());
}
