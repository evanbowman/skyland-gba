////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "notificationScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr NotificationScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.player().button_down(Button::action_1) or
        APP.player().button_down(Button::action_2) or
        APP.player().button_down(Button::left) or
        APP.player().button_down(Button::right) or
        APP.player().button_down(Button::up) or
        APP.player().button_down(Button::down)) {

        return next_scene_();
    }

    return null_scene();
}



void NotificationScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    description_.emplace(OverlayCoord{0, u8(calc_screen_tiles().y - 1)});

    description_->assign(msg_.c_str());

    for (int i = 0; i < description_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 18, 425);
    }
}



void NotificationScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    description_.reset();
    PLATFORM.fill_overlay(0);
}



} // namespace skyland
