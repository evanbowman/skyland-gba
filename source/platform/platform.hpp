#pragma once


#include "dateTime.hpp"
#include "function.hpp"
#include "graphics/contrast.hpp"
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


struct FontColors {
    ColorConstant foreground_;
    ColorConstant background_;
};


template <typename T> class Vector;


// Anything platform specific should be defined here.


////////////////////////////////////////////////////////////////////////////////
// Platform
////////////////////////////////////////////////////////////////////////////////


class Platform {
public:
    class Screen;
    class Keyboard;
    class Logger;
    class Speaker;
    class NetworkPeer;
    class DeltaClock;
    class SystemClock;
    class RemoteConsole;

    using DeviceName = StringBuffer<23>;
    DeviceName device_name() const;


    struct alignas(4) EncodedTile {
        u8 bytes_[128];
    };


    EncodedTile encode_tile(u8 tile_data[16][16]);


    void overwrite_t0_tile(u16 index, const EncodedTile& t);
    void overwrite_t1_tile(u16 index, const EncodedTile& t);

    void overwrite_sprite_tile(u16 index, const EncodedTile& t);


    struct TilePixels {
        u8 data_[16][16];
    };


    TilePixels extract_tile(Layer layer, u16 tile);


    // Timestamp recorded when the process launched, returns an empty optional
    // if the platform does not provide a functional clock.
    std::optional<DateTime> startup_time() const;

    Screen& screen()
    {
        return screen_;
    }


    // For devices with two screens, like the nintendo ds.
    Screen* sub_screen();


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

    SystemClock& system_clock()
    {
        return system_clock_;
    }

    // NOTE: I must admit, the platform class interface has become quite
    // bloated.

    const char* get_opt(char opt);

    // On some platforms, fatal() will trigger a soft reset. But soft-reset is
    // mostly reserved for bare-metal platforms, where it's usually easy to
    // perform a soft reset via a simple BIOS call to reset the ROM. When
    // running as a windowed application within an OS, though, it's not as
    // simple to reset a process back to its original state at time of creation,
    // so it's easier in those cases to just exit. In any event, fatal() does
    // not return.
    [[noreturn]] static void fatal(const char* message);


    [[noreturn]] static void restart();


    // Enable platform specific features. NOP if unsupported.
    void* system_call(const char* feature_name, void* arg);


    struct TextureMapping {
        const char* texture_name_;
        u16 offset_;
    };

    // Supplied with a unicode codepoint, this function should provide an offset
    // into a texture image from which to load a glyph image.
    using TextureCpMapper =
        std::optional<TextureMapping> (*)(const utf8::Codepoint&);

    // Map a glyph into the vram space reserved for the overlay tile layer.
    TileDesc map_glyph(const utf8::Codepoint& glyph,
                       const TextureMapping& mapping);


    // Copy a chunk of the overlay texture into another chunk of the
    // texture. Allows you to create textures larger than will fit in vram, and
    // swap in sections.
    void load_overlay_chunk(TileDesc dst, TileDesc src, u16 count);


    static constexpr const int dynamic_texture_count = 6;

    // While spritesheets are generally fixed, the platform provides a number of
    // flexible texture indices, allowing memory to be mapped dynamically into
    // video ram. By calling DynamicTexture::remap(), the platform will map the
    // requested texture index into video memory, and the index may then be used
    // when drawing sprites.
    class DynamicTexture {
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


    std::optional<DynamicTexturePtr> make_dynamic_texture();


    // In glyph mode, the platform will automatically unmap glyphs when their
    // tiles are overwritten by set_tile.
    void enable_glyph_mode(bool enabled);

    // In this mode, the engine will use all of available overlay vram for
    // rendering text glyphs. Added during chinese language localization,
    // when we needed extra memory for displaying large amounts of text.
    void enable_expanded_glyph_mode(bool enabled);


    // NOTE: For the overlay and background, the tile layers consist of 32x32
    // tiles, where each tiles is 8x8 pixels. The overlay and the background
    // wrap when scrolled. Map tiles, on the other hand, are 32x24 pixels, and
    // the whole map consists of 64x64 8x8 pixel tiles.
    void set_tile(Layer layer,
                  u16 x,
                  u16 y,
                  TileDesc val,
                  std::optional<u16> palette = {});


    void set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val);


    void set_palette(Layer layer, u16 x, u16 y, u16 palette);
    u16 get_palette(Layer layer, u16 x, u16 y);


    void set_scroll(Layer layer, u16 x, u16 y);


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


    bool overlay_texture_exists(const char* name);

    // Sleep halts the game for an amount of time equal to some number
    // of game updates. Given that the game should be running at
    // 60fps, one update equals 1/60 of a second.
    using Frame = u32;
    void sleep(Frame frames);


    bool is_running() const;


    // If the watchdog is not fed every ten seconds, the game will reset itself,
    // after calling the user-supplied watchdog handler (obviously, don't spend
    // more than ten seconds in the watchdog handler!).
    void feed_watchdog();

    using UnrecoverrableErrorCallback = Function<16, void(Platform& pfrm)>;
    void on_unrecoverrable_error(UnrecoverrableErrorCallback callback);


    // Not implemented for all platforms. If unimplemented, the funciton will
    // simply return immediately. For handheld consoles without an operating
    // system, where the only way that you can shutdown the system is by
    // flipping the power switch, it doesn't make sense to implement an exit
    // function.
    void soft_exit();

    bool write_save_data(const void* data, u32 length, u32 offset);
    bool read_save_data(void* buffer, u32 data_length, u32 offset);

    int save_capacity();



    // For historical reasons, allows you to specify a folder and filename
    // separately. If you do not care for this behavior, supply an empty string
    // in the folder argument, and the path in the filename argument.
    const char* load_file_contents(const char* folder,
                                   const char* filename) const;


    // On supported platforms, performs a stack overflow check. Otherwise, a
    // no-op.
    static void stackcheck();


    void walk_filesystem(Function<32, void(const char* path)>);


    // Scratch buffers are sort of a blunt instrument. Designed for uncommon
    // scenarios where you need a lot of memory. The platform provides one
    // hundred scratch buffers to work with. The Rc wrapper will automatically
    // deallocate buffers when you're done with them. Creating a new scratch
    // buffer when the buffer pool is exhausted will cause the system to lock
    // up, so do not try to hold more than one hundred active references to
    // scratch buffers (not sure why you would even need 200kB of temporary
    // scratch space anyway...).
    ScratchBufferPtr make_scratch_buffer();


    // An emergency function to invoke when the system runs out of scratch
    // buffers. This function should, if possible, drop any non-essential
    // references to scratch buffers.
    void set_scratch_buffer_oom_handler(Function<16, void()> callback);


    int scratch_buffers_remaining();


    ////////////////////////////////////////////////////////////////////////////
    // DeltaClock
    ////////////////////////////////////////////////////////////////////////////


    class DeltaClock {
    public:
        ~DeltaClock();

        Microseconds reset();


        using TimePoint = int;

        TimePoint sample() const;


        static Microseconds duration(TimePoint t1, TimePoint t2);


    private:
        friend class Platform;

        DeltaClock();

        void* impl_;
    };


    ////////////////////////////////////////////////////////////////////////////
    // SystemClock
    ////////////////////////////////////////////////////////////////////////////


    class SystemClock {
    public:
        // NOTE: SystemClock::now() sometimes causes minor audio bugs on the
        // GAMEBOY ADVANCE. In the meantime, use Platform::startup_time() to
        // access an initial startup timestamp, and only call DateTime::now()
        // during shutdown, when saving a game.
        std::optional<DateTime> now();

    private:
        friend class Platform;

        void init(Platform& pfrm);

        SystemClock();
    };


    ////////////////////////////////////////////////////////////////////////////
    // Screen
    ////////////////////////////////////////////////////////////////////////////



    class Screen {
    public:
        static constexpr u32 sprite_limit = 128;


        class Touch {
        public:
            std::optional<Vec2<u32>> read() const
            {
                return current_;
            }


            std::optional<Vec2<u32>> up_transition() const
            {
                if (not current_ and previous_) {
                    return previous_;
                }

                return {};
            }


        private:
            friend class Screen;
            std::optional<Vec2<u32>> current_;
            std::optional<Vec2<u32>> previous_;
        };


        const Touch* touch() const;


        void draw(const Sprite& spr);

        void clear();

        void display();

        Vec2<u32> size() const;

        void set_contrast(Contrast contrast);

        Contrast get_contrast() const;

        using Shader = ColorConstant (*)(int, ColorConstant, int);

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
                  std::optional<ColorConstant> base = {},
                  bool include_sprites = true,
                  bool include_overlay = false);


        void schedule_fade(Float amount,
                           ColorConstant color = ColorConstant::rich_black,
                           bool include_sprites = true,
                           bool include_overlay = false);


        void pixelate(u8 amount,
                      bool include_overlay = true,
                      bool include_background = true,
                      bool include_sprites = true);

    private:
        Screen();

        friend class Platform;

        View view_;
        void* userdata_;
        Touch touch_;
    };


    ////////////////////////////////////////////////////////////////////////////
    // Keyboard
    ////////////////////////////////////////////////////////////////////////////


    class Keyboard {
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


        struct ControllerInfo {
            int vendor_id;
            int product_id;
            int action_1_key;
            int action_2_key;
            int start_key;
            int alt_1_key;
            int alt_2_key;
        };

        void register_controller(const ControllerInfo& info);

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


    class Logger {
    public:
        void log(Severity severity, const char* msg);

        void flush();

        Vector<char>* data();

        void set_threshold(Severity severity);

    private:
        Logger();

        friend class Platform;
    };


    ////////////////////////////////////////////////////////////////////////////
    // Speaker
    ////////////////////////////////////////////////////////////////////////////


    class Speaker {
    public:


        enum Note : u8 {
            C = 0,
            CIS,
            D,
            DIS,
            E,
            F,
            FIS,
            G,
            GIS,
            A,
            BES,
            B,
            invalid,
        };


        enum class Channel {
            square_1,
            wave,
            noise,
            square_2,
            invalid,
        };


        void play_chiptune_note(Channel channel, Note note, u8 octave);


        enum class Effect {
            none,
            vibrato,
        };


        void apply_chiptune_effect(Channel channel,
                                   Effect effect,
                                   u8 argument);


        struct ChannelSettings {
            u8 length_ : 6;
            u8 duty_ : 2;
            u8 envelope_step_ : 3;
            u8 envelope_direction_ : 1;
            u8 volume_ : 4;
        };


        void init_chiptune_square_1(ChannelSettings settings);
        void init_chiptune_square_2(ChannelSettings settings);
        void init_chiptune_wave(u16 config);
        void init_chiptune_noise(ChannelSettings settings);


        // NOTE: All music will loop. It's just more efficient to implement the
        // music such that all tracks are either looped or non-looping, and I
        // decided to make tracks loop. If you want music to stop when finished,
        // stop it yourself.
        void play_music(const char* name, Microseconds offset);
        void stop_music();

        void halt_music();
        void resume_music();

        // By convention, volume ranges from zero to nineteen (twenty volume
        // levels). Maximum performance when volume is zero or nineteen.

        static const u8 music_volume_max = 19;

        void set_music_volume(u8 volume);

        void set_music_reversed(bool reversed);


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
                        std::optional<Vec2<Float>> position = {});
        bool is_sound_playing(const char* name);

        void clear_sounds();

        // Updates the listener position for spatialized audio, if supported.
        void set_position(const Vec2<Float>& position);

        Microseconds track_length(const char* sound_or_music_name);

    private:
        friend class Platform;

        Speaker();
    };


    ////////////////////////////////////////////////////////////////////////////
    // NetworkPeer
    ////////////////////////////////////////////////////////////////////////////


    class NetworkPeer {
    public:
        NetworkPeer();
        NetworkPeer(const NetworkPeer&) = delete;
        ~NetworkPeer();

        void connect(const char* peer_address);
        void listen();

        void disconnect();

        bool is_connected() const;
        bool is_host() const;

        struct Message {
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

        void update();

        // The result of poll-message will include the length of the available
        // data in the network-peer's buffer. If the space in the buffer is
        // insufficient to frame a message, exit polling, and do not call
        // poll_consume() until there's enough space to fill an entire message.
        std::optional<Message> poll_message();
        void poll_consume(u32 length);

        // Will return false if the platform does not support networked
        // multiplayer.
        static bool supported_by_device();

        struct Stats {
            int transmit_count_;
            int receive_count_;
            int transmit_loss_;
            int receive_loss_;
            int link_saturation_; // percentage 0 to 100
        };

        Stats stats();

    private:
        void* impl_;
    };


    ////////////////////////////////////////////////////////////////////////////
    // RemoteConsole
    ////////////////////////////////////////////////////////////////////////////


    class RemoteConsole {
    public:
#if defined(__GBA__) or defined(__NDS__)
        using Line = StringBuffer<1956>;
#else
        using Line = StringBuffer<8000>;
#endif
        std::optional<Line> readline();

        bool printline(const char* text, bool show_prompt = true);

        void printline_blocking(const char* text, bool show_prompt = true)
        {
            while (not printline(text, show_prompt))
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

private:
    Platform();

    friend int main(int argc, char** argv);

    SystemClock system_clock_;
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
ColorConstant passthrough_shader(int palette, ColorConstant k, int arg);
ColorConstant grayscale_shader(int palette, ColorConstant k, int arg);
ColorConstant contrast_shader(int palette, ColorConstant k, int arg);



class SynchronizedBase {
protected:
    void init(Platform& pf);
    void lock();
    void unlock();

    ~SynchronizedBase();

private:
    friend class Platform;

    void* impl_;
};


template <typename T> class Synchronized : SynchronizedBase {
public:
    template <typename... Args>
    Synchronized(Platform& pf, Args&&... args)
        : data_(std::forward<Args>(args)...)
    {
        init(pf);
    }

    template <typename F> void acquire(F&& handler)
    {
        lock();
        handler(data_);
        unlock();
    }

private:
    T data_;
};


// Helper function for drawing background tiles larger than the default size (8x8 pixels)
inline void draw_image(Platform& pfrm,
                       TileDesc start_tile,
                       u16 start_x,
                       u16 start_y,
                       u16 width,
                       u16 height,
                       Layer layer)
{

    u16 tile = start_tile;

    for (u16 y = start_y; y < start_y + height; ++y) {
        for (u16 x = start_x; x < start_x + width; ++x) {
            pfrm.set_tile(layer, x, y, tile++);
        }
    }
}


template <Key k> bool key_down(Platform& pfrm)
{
    return pfrm.keyboard().down_transition<k>();
}


inline void debug(Platform& pf, const char* msg)
{
    pf.logger().log(Severity::debug, msg);
}
inline void info(Platform& pf, const char* msg)
{
    pf.logger().log(Severity::info, msg);
}
inline void warning(Platform& pf, const char* msg)
{
    pf.logger().log(Severity::warning, msg);
}
inline void error(Platform& pf, const char* msg)
{
    pf.logger().log(Severity::error, msg);
}

template <u32 size> void debug(Platform& pf, const StringBuffer<size>& buffer)
{
    pf.logger().log(Severity::debug, buffer.c_str());
}
template <u32 size> void info(Platform& pf, const StringBuffer<size>& buffer)
{
    pf.logger().log(Severity::debug, buffer.c_str());
}
template <u32 size> void warning(Platform& pf, const StringBuffer<size>& buffer)
{
    pf.logger().log(Severity::debug, buffer.c_str());
}
template <u32 size> void error(Platform& pf, const StringBuffer<size>& buffer)
{
    pf.logger().log(Severity::debug, buffer.c_str());
}
