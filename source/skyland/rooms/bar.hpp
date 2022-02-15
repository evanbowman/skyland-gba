#pragma once

#include "decoration.hpp"
#include "skyland/tile.hpp"
#include "synth.hpp"



// A musical bar, integrating four synth channels. Each synth channel sits atop
// a bar in a slot corresponding to an instrument. Designed to make use of the
// DMG sound channels on the gba platform, or other platforms that support
// chiptunes.



namespace skyland {



class Bar : public Decoration {
public:

    Bar(Island* parent, const Vec2<u8>& position);


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    static u32 properties()
    {
        return Decoration::properties() & ~RoomProperties::locked_by_default;
    }


    static const char* name()
    {
        return "measure";
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    void play(Platform& pfrm);


    void reset();


    void finalize(Platform& pfrm, App& app) override;


private:

    Synth* square_1() const;
    Synth* square_2() const;
    Synth* wave() const;
    Synth* noise() const;

    enum class CommandType {
        none,
        envelope,
        wave,
        vibrato,
    };

    // Two bits per command enum, 16 notes per channel, four channels
    Bitvector<2 * 16 * 4> command_flags_;

    Platform::Speaker::ChannelSettings square_1_settings_;
    Platform::Speaker::ChannelSettings square_2_settings_;
    Platform::Speaker::ChannelSettings noise_settings_;
    u16 wave_settings_;

    // Bar currently playing
    u8 playing_ : 1;

    u8 unused_ : 7;

    // Index into the list of notes, 0->16
    u8 index_ = 0;

    Microseconds timer_ = 0;
};



}
