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


#include "notificationScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr<Scene> NotificationScene::update(Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.player().key_down(Key::action_1) or
        APP.player().key_down(Key::action_2) or
        APP.player().key_down(Key::left) or APP.player().key_down(Key::right) or
        APP.player().key_down(Key::up) or APP.player().key_down(Key::down)) {

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
