#pragma once

#include "decoration.hpp"
#include "platform/platform.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Speaker;



class Synth : public Decoration
{
public:
    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    Synth(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    static u32 properties()
    {
        return Decoration::properties();
    }


    static const char* name()
    {
        return "synth";
    }


    static SystemString ui_name()
    {
        return SystemString::block_synth;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1768;
    }


    static Icon unsel_icon()
    {
        return 1784;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;


    bool non_owner_selectable() const override
    {
        return true;
    }


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const Vec2<u8>& cursor) override;


    using Note = Platform::Speaker::NoteDesc;


    struct EffectParameter
    {
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


    Speaker* speaker() const;


private:
    EffectParameter effect_parameters_[16];
    Note notes_[16];
};



} // namespace skyland
