////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "notificationScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr<Scene>
NotificationScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }

    if (app.player().key_down(pfrm, Key::action_1) or
        app.player().key_down(pfrm, Key::action_2) or
        app.player().key_down(pfrm, Key::left) or
        app.player().key_down(pfrm, Key::right) or
        app.player().key_down(pfrm, Key::up) or
        app.player().key_down(pfrm, Key::down)) {

        return next_scene_();
    }

    return null_scene();
}



void NotificationScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    description_.emplace(pfrm,
                         OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});

    description_->assign(msg_.c_str());

    for (int i = 0; i < description_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 425);
    }
}



void NotificationScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    description_.reset();
    pfrm.fill_overlay(0);
}



} // namespace skyland
