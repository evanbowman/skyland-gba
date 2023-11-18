////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
    void enter(App& app, Scene& prev) override
    {
        no_text_.emplace(SYSTR(no)->c_str(), OverlayCoord{3, 5});
        yes_text_.emplace(SYSTR(yes)->c_str(), OverlayCoord{3, 7});
        PLATFORM.set_tile(Layer::overlay, 1, 5, 396);
        PLATFORM.set_tile(Layer::overlay, 1, 7, 0);
    }


    void exit(App& app, Scene& next) override
    {
        no_text_.reset();
        yes_text_.reset();
        PLATFORM.fill_overlay(0);
    }


    ScenePtr<Scene> update(App& app, Microseconds delta) override
    {
        if (PLATFORM.keyboard().down_transition(Key::action_1)) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
            if (selection_) {
                macrocosm(app).save();
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
    std::optional<Text> no_text_;
    std::optional<Text> yes_text_;
};



} // namespace skyland::macro
