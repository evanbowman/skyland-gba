////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "graphics/overlay.hpp"
#include "platform/platform.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"
#include "startMenuScene.hpp"



namespace skyland
{



class HibernateScene : public Scene
{
public:
    void enter(Scene& prev) override
    {
        text_.emplace();
        text_->assign(SYSTR(misc_hibernate_message)->c_str(), {1, 4}, {28, 8});
    }


    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);
    }


    ScenePtr update(Time delta) override
    {
        if (key_down<Key::action_1>()) {
            text_.reset();
            PLATFORM.screen().display();
            info("enter hibernate...");
            PLATFORM_EXTENSION(hibernate);
            info("resume!");
            return make_scene<StartMenuScene>(1);
        }

        return null_scene();
    }


private:
    Optional<TextView> text_;
};



} // namespace skyland
