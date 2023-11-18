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


#include "fatalErrorScene.hpp"
#include "loadLevelScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void FatalErrorScene::enter(App& app, Scene& prev)
{
    if (app.game_mode() not_eq App::GameMode::adventure) {
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



void FatalErrorScene::exit(App&, Scene& next)
{
    verbose_error_.reset();
}



ScenePtr<Scene> FatalErrorScene::update(App& app, Microseconds dt)
{
    app.player().update(app, dt);

    if (app.player().key_down(Key::action_2)) {
        PLATFORM.fill_overlay(0);
        app.restore_backup();
        PLATFORM.speaker().clear_sounds();
        return scene_pool::alloc<LoadLevelScene>();
    }
    return null_scene();
}



} // namespace skyland
