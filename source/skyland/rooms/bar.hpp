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

    Synth* pulse_1() const;
    Synth* pulse_2() const;
    Synth* wave() const;
    Synth* noise() const;

    // Bar currently playing
    u8 playing_ : 1;

    // Number of repetitions
    u8 repeat_ : 7;

    // Index into the list of notes, 0->16
    u8 index_ = 0;

    std::optional<Vec2<u8>> next_bar_;
    Microseconds timer_ = 0;
};



}
