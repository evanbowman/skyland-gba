#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class RewindScene : public Scene
{
public:
    RewindScene(bool is_far_camera) : far_camera_(is_far_camera)
    {
    }

    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


    void display(Platform&, App&) override;


private:
    void print_timestamp(Platform& pfrm, App& app);


    std::optional<Text> text_;
    bool far_camera_;
};



} // namespace skyland
