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


#include "fatalErrorScene.hpp"
#include "loadLevelScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void FatalErrorScene::enter(Scene& prev)
{
    if (APP.game_mode() not_eq App::GameMode::adventure) {
        Platform::fatal(message_.c_str());
    }

    const auto bkg_color = custom_color(0xcb1500);
    PLATFORM.screen().schedule_fade(1.f, bkg_color);

    static constexpr const Text::OptColors text_colors{
        {custom_color(0xffffff), bkg_color}};

    static constexpr const Text::OptColors text_colors_inv{
        {text_colors->background_, text_colors->foreground_}};

    Text::print("fatal error", {1, 1}, text_colors_inv);

    verbose_error_.emplace();
    verbose_error_->assign(message_.c_str(), {1, 3}, {28, 15}, 0, text_colors);

    Text::print("please contact developers!", {1, 16}, text_colors_inv);

    Text::print("press B to retry level", {1, 18}, text_colors_inv);
}



void FatalErrorScene::exit(Scene& next)
{
    verbose_error_.reset();
}



ScenePtr<Scene> FatalErrorScene::update(Microseconds dt)
{
    APP.player().update(dt);

    if (APP.player().key_down(Key::action_2)) {
        PLATFORM.fill_overlay(0);
        APP.restore_backup();
        PLATFORM.speaker().clear_sounds();
        return scene_pool::alloc<LoadLevelScene>();
    }
    return null_scene();
}



} // namespace skyland
