#pragma once

#include "decoration.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class Synth : public Decoration {
public:

    Synth(Island* parent, const Vec2<u8>& position);


    static u32 properties()
    {
        return Decoration::properties() & ~RoomProperties::locked_by_default
            ;
    }


    static const char* name()
    {
        return "synth";
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


    struct Note {
        u8 note_ : 4;
        u8 octave_ : 4;
    };


    const Note* notes() const
    {
        return notes_;
    }


private:

    struct Settings {
        u32 reserved_ : 32;
    } settings_;

    Note notes_[16];

};



}
