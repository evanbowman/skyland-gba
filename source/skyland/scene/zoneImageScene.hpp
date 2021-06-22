#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class ZoneImageScene : public Scene {
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    Microseconds timer_ = 0;

    enum class State {
        fade_in,
        wait,
        fade_out,
    } state_ = State::fade_in;

    std::optional<Text> text_;
};



} // namespace skyland
