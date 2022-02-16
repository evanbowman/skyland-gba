#pragma once

#include "decoration.hpp"
#include "skyland/tile.hpp"
#include "platform/platform.hpp"



namespace skyland {



class Measure;



class Synth : public Decoration {
public:


    static void format_description(StringBuffer<512>& buffer);


    Synth(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


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
        Platform::Speaker::Note note_ : 4;
        u8 octave_ : 4;
    };


    struct EffectParameter {
        u8 value_;
    };


    Note* notes()
    {
        return notes_;
    }


    EffectParameter* effect_parameters()
    {
        return effect_parameters_;
    }


    Platform::Speaker::Channel channel() const;


    Measure* measure() const;


private:


    EffectParameter effect_parameters_[16];
    Note notes_[16];

};



}
