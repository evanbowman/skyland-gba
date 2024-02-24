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


#include "selectorScene.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene/startMenuScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



class SaveConfirmScene : public Scene
{
public:
    void enter(Scene& prev) override
    {
        no_text_.emplace(SYSTR(no)->c_str(), OverlayCoord{3, 5});
        yes_text_.emplace(SYSTR(yes)->c_str(), OverlayCoord{3, 7});
        PLATFORM.set_tile(Layer::overlay, 1, 5, 396);
        PLATFORM.set_tile(Layer::overlay, 1, 7, 0);
    }


    void exit(Scene& next) override
    {
        no_text_.reset();
        yes_text_.reset();
        PLATFORM.fill_overlay(0);
    }


    ScenePtr<Scene> update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition(Key::action_1)) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
            if (selection_) {
                macrocosm().save();
                PLATFORM.screen().schedule_fade(0.f);
                return scene_pool::alloc<SelectorScene>();
            } else {
                return scene_pool::alloc<StartMenuScene>(1);
            }
        }

        if (PLATFORM.keyboard().down_transition(Key::down) or
            PLATFORM.keyboard().down_transition(Key::up)) {
            selection_ = not selection_;
            PLATFORM.speaker().play_sound("click_wooden", 2);
            if (not selection_) {
                PLATFORM.set_tile(Layer::overlay, 1, 5, 396);
                PLATFORM.set_tile(Layer::overlay, 1, 7, 0);
            } else {
                PLATFORM.set_tile(Layer::overlay, 1, 5, 0);
                PLATFORM.set_tile(Layer::overlay, 1, 7, 396);
            }
        }

        return null_scene();
    }



private:
    bool selection_ = false;
    Optional<Text> no_text_;
    Optional<Text> yes_text_;
};



} // namespace skyland::macro
