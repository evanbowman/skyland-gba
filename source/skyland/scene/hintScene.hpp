#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class HintScene : public Scene {
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;

    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

private:
    u32 hint_index_ = 0;

    enum class State {
        scene_intro,
        // scene_exit,
        fade_out,
        swap_img,
        fade_in,
        idle,
    } state_ = State::scene_intro;

    Microseconds timer_ = 0;

    std::optional<Text> heading_;
    std::optional<TextView> body_;

    Buffer<u8, 24> index_sequence_;
};



} // namespace skyland
