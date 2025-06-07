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


    ScenePtr update(Time delta) override
    {
        if (APP.player().key_down(Key::up)) {
            selection_ = false;
            yes_text_->assign(SYSTR(yes)->c_str());
            no_text_->assign(SYSTR(no)->c_str(), sel_colors);
        }

        if (APP.player().key_down(Key::down)) {
            selection_ = true;
            yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
            no_text_->assign(SYSTR(no)->c_str());
        }

        if (APP.player().key_down(Key::action_1)) {
            if (selection_) {
                if (APP.opponent_island()) {
                    APP.swap_opponent<FriendlyAI>();
                    for (auto& r : APP.opponent_island()->rooms()) {
                        r->unset_target();
                    }
                }
                APP.exit_condition() = App::ExitCondition::defeat;
                PLATFORM.speaker().stop_music();
            }
            return make_scene<ReadyScene>();
        }

        return null_scene();
    }


    void enter(Scene& prev) override
    {
        msg_.emplace(SYSTR(are_you_sure)->c_str(), OverlayCoord{1, 3});
        no_text_.emplace(OverlayCoord{2, 5});
        yes_text_.emplace(SYSTR(yes)->c_str(), OverlayCoord{2, 7});

        no_text_->assign(SYSTR(no)->c_str(), sel_colors);
    }


    void exit(Scene& next) override
    {
        msg_.reset();
        yes_text_.reset();
        no_text_.reset();
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.fill_overlay(0);
    }


private:
    bool selection_ = false;

    Optional<Text> msg_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;
};



} // namespace skyland
