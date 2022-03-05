#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class IntroCreditsScene : public Scene
{
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    static void show_sunflowers(Platform& pfrm, int scroll, Float darken);


private:
    bool wait_ = true;
    bool exit_ = false;
    std::optional<Text> text_;
    std::optional<Text> copyright_text_;
    Microseconds timer_ = 0;
    Microseconds flower_effect_timer_ = 0;
};



} // namespace skyland
