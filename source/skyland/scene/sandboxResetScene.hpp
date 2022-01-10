#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class SandboxResetScene : public Scene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


private:
    bool selection_ = true;

    std::optional<Text> msg_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
};



} // namespace skyland
