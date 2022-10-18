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


#include "graphics/overlay.hpp"
#include "readyScene.hpp"
#include "skyland/scene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class SurrenderConfirmScene : public Scene
{
public:
    static constexpr const auto sel_colors =
        FontColors{custom_color(0x000010), custom_color(0xffffff)};


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (app.player().key_down(pfrm, Key::up)) {
            selection_ = false;
            yes_text_->assign(SYSTR(yes)->c_str());
            no_text_->assign(SYSTR(no)->c_str(), sel_colors);
        }

        if (app.player().key_down(pfrm, Key::down)) {
            selection_ = true;
            yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
            no_text_->assign(SYSTR(no)->c_str());
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            if (selection_) {
                app.exit_condition() = App::ExitCondition::defeat;
                pfrm.speaker().stop_music();
            }
            return scene_pool::alloc<ReadyScene>();
        }

        return null_scene();
    }


    void enter(Platform& pfrm, App&, Scene& prev) override
    {
        msg_.emplace(pfrm, SYSTR(are_you_sure)->c_str(), OverlayCoord{1, 3});
        no_text_.emplace(pfrm, OverlayCoord{2, 5});
        yes_text_.emplace(pfrm, SYSTR(yes)->c_str(), OverlayCoord{2, 7});

        no_text_->assign(SYSTR(no)->c_str(), sel_colors);
    }


    void exit(Platform& pfrm, App&, Scene& next) override
    {
        msg_.reset();
        yes_text_.reset();
        no_text_.reset();
        pfrm.screen().schedule_fade(0);
        pfrm.fill_overlay(0);
    }


private:
    bool selection_ = false;

    std::optional<Text> msg_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
};



} // namespace skyland
