#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class AdventureModeSettingsScene : public Scene {
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    std::optional<Text> difficulty_text_;
    std::optional<Text> easy_text_;
    std::optional<Text> normal_text_;
    std::optional<Text> hard_text_;
};



} // namespace skyland
