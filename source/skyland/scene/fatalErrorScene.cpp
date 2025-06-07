////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



ScenePtr FatalErrorScene::update(Time dt)
{
    APP.player().update(dt);

    if (APP.player().key_down(Key::action_2)) {
        PLATFORM.fill_overlay(0);
        APP.restore_backup();
        PLATFORM.speaker().clear_sounds();
        return make_scene<LoadLevelScene>();
    }
    return null_scene();
}



} // namespace skyland
