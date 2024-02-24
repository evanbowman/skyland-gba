////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    ScenePtr<Scene> update(Time delta) override
    {
        if (key_down<Key::action_1>()) {
            text_.reset();
            PLATFORM.screen().display();
            info("enter hibernate...");
            PLATFORM.system_call("hibernate", nullptr);
            info("resume!");
            return scene_pool::alloc<StartMenuScene>(1);
        }

        return null_scene();
    }


private:
    Optional<TextView> text_;
};



} // namespace skyland
