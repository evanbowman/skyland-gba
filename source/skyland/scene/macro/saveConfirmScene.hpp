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


    ScenePtr update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition(Key::action_1)) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
            if (selection_) {
                macrocosm().save();
                PLATFORM.screen().schedule_fade(0.f);
                return make_scene<SelectorScene>();
            } else {
                return make_scene<StartMenuScene>(1);
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
