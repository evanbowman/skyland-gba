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


    Vec2<u8> cursor_;

    std::optional<Text> heading_;

    Synth::Note notes_[16];
    Synth::Command commands_[16];

    Vec2<u8> synth_pos_;
    bool synth_near_;

    Platform::Speaker::Channel channel_;
};



}
