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


#include "modifierKeyHintScene.hpp"
#include "assignWeaponGroupScene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



ScenePtr<Scene> update_modifier_keys();



ScenePtr<Scene> ModifierKeyHintScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (not player().key_pressed(Key::start)) {
        return scene_pool::alloc<ReadyScene>();
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

    PLATFORM.screen().schedule_fade(0.f);

    PLATFORM.fill_overlay(0);
}



} // namespace skyland
