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


#include "notificationScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr<Scene> NotificationScene::update(Time delta)
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
