////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "modifierKeyHintScene.hpp"
#include "assignWeaponGroupScene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



ScenePtr update_modifier_keys();



ScenePtr ModifierKeyHintScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (not player().key_pressed(Key::start)) {
        return make_scene<ReadyScene>();
    }

    if (auto scene = update_modifier_keys()) {
        return scene;
    }

    return null_scene();
}



void ModifierKeyHintScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    const auto st = calc_screen_tiles();

    for (int x = 3; x < st.x - 3; ++x) {
        for (int y = 2; y < st.y - 2; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 112);
        }
    }

    PLATFORM.set_tile(Layer::overlay, 4, 6, 392);
    PLATFORM.set_tile(Layer::overlay, 4, 8, 393);
    PLATFORM.set_tile(Layer::overlay, 4, 10, 394);
    PLATFORM.set_tile(Layer::overlay, 4, 12, 395);

    Text b_opt("/B", OverlayCoord{5, 6});
    b_opt.__detach();

    Text a_opt("A", OverlayCoord({4, 14}));
    a_opt.__detach();

    auto title = SYSTR(modifier_keys_title);
    title_.emplace(

        title->c_str(),
        OverlayCoord{(u8)centered_text_margins(utf8::len(title->c_str())), 3});

    text_.emplace_back(SYSTR(modifier_keys_opt_1)->c_str(), OverlayCoord{8, 6});

    text_.emplace_back(SYSTR(modifier_keys_opt_2)->c_str(), OverlayCoord{8, 8});

    text_.emplace_back(SYSTR(modifier_keys_opt_3)->c_str(),
                       OverlayCoord{8, 10});

    text_.emplace_back(SYSTR(modifier_keys_opt_4)->c_str(),
                       OverlayCoord{8, 12});

    text_.emplace_back(SYSTR(modifier_keys_opt_5)->c_str(),
                       OverlayCoord{8, 14});

    text_.emplace_back(SYSTR(modifier_keys_opt_6)->c_str(),
                       OverlayCoord{4, 16});

    PLATFORM.screen().schedule_fade(0.5f);
}



void ModifierKeyHintScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    text_.clear();

    PLATFORM.screen().schedule_fade(0.f);

    PLATFORM.fill_overlay(0);
}



} // namespace skyland
