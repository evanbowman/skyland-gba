#pragma once

#include "graphics/overlay.hpp"
#include "platform/platform.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"
#include "startMenuScene.hpp"



namespace skyland {



class HibernateScene : public Scene {
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        text_.emplace(pfrm);
        text_->assign(SYSTR(misc_hibernate_message)->c_str(), {1, 4}, {28, 8});
    }


    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        pfrm.fill_overlay(0);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ += delta;

        if (timer_ > seconds(6)) {
            text_.reset();
            pfrm.screen().display();
            pfrm.hibernate();
            return scene_pool::alloc<StartMenuScene>(pfrm, 1);
        }

        return null_scene();
    }


private:
    std::optional<TextView> text_;
    Microseconds timer_ = 0;
};



} // namespace skyland
