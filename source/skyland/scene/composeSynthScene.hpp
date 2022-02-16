#pragma once


#include "skyland/rooms/synth.hpp"
#include "worldScene.hpp"



namespace skyland {




class ComposeSynthScene : public ActiveWorldScene {
public:
    ComposeSynthScene(App& app, Synth& synth);


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:

    void repaint(Platform&);


    void demo_note(Platform&);


    Vec2<u8> cursor_;

    std::optional<Text> heading_;

    Synth::Note notes_[16];
    Synth::EffectParameter effect_parameters_[16];

    Vec2<u8> synth_pos_;
    bool synth_near_;

    bool init_ = true;

    Platform::Speaker::ChannelSettings square_1_settings_;
    Platform::Speaker::ChannelSettings square_2_settings_;
    Platform::Speaker::ChannelSettings noise_settings_;
    u16 wave_settings_;


    Platform::Speaker::Channel channel_;
};



}
