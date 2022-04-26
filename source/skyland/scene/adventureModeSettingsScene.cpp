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


#include "adventureModeSettingsScene.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland
{



void AdventureModeSettingsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    const char* difficulty_str = "difficulty:";

    difficulty_text_.emplace(
        pfrm,
        difficulty_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(difficulty_str)),
                     1});


    const char* easy_str = "easy";

    easy_text_.emplace(
        pfrm,
        easy_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(easy_str)), 4});


    const char* normal_str = "normal";

    normal_text_.emplace(
        pfrm,
        normal_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(normal_str)), 6});


    const char* hard_str = "hard";

    hard_text_.emplace(
        pfrm,
        hard_str,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(hard_str)), 8});


    app.persistent_data().difficulty_ = PersistentData::Difficulty::experienced;

    pfrm.screen().fade(0.96f);
    pfrm.screen().fade(1.f);
}



void AdventureModeSettingsScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    difficulty_text_.reset();
    easy_text_.reset();
    normal_text_.reset();
    hard_text_.reset();
}



ScenePtr<Scene>
AdventureModeSettingsScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::up)) {
        auto& diff = app.persistent_data().difficulty_;
        diff = (PersistentData::Difficulty)(((u8)diff - 1) % 3);
        pfrm.speaker().play_sound("click_wooden", 2);
    }

    if (app.player().key_down(pfrm, Key::down)) {
        auto& diff = app.persistent_data().difficulty_;
        diff = (PersistentData::Difficulty)(((u8)diff + 1) % 3);
        pfrm.speaker().play_sound("click_wooden", 2);
    }

    auto sel = [&pfrm](auto& text, int tile)
               {
                   pfrm.set_tile(Layer::overlay, text->coord().x - 2, text->coord().y, tile);
               };

    switch (app.persistent_data().difficulty_) {
    case PersistentData::Difficulty::beginner:
        sel(easy_text_, 396);
        sel(normal_text_, 0);
        sel(hard_text_, 0);
        break;

    case PersistentData::Difficulty::experienced:
        sel(easy_text_, 0);
        sel(normal_text_, 396);
        sel(hard_text_, 0);
        break;

    case PersistentData::Difficulty::expert:
        sel(easy_text_, 0);
        sel(normal_text_, 0);
        sel(hard_text_, 396);
        break;
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        pfrm.speaker().play_sound("button_wooden", 3);
        switch (app.persistent_data().difficulty_) {
        case PersistentData::Difficulty::beginner:
            app.invoke_script(pfrm, "/scripts/config/easy/score.lisp");
            break;

        case PersistentData::Difficulty::experienced:
            app.invoke_script(pfrm, "/scripts/config/normal/score.lisp");
            break;

        case PersistentData::Difficulty::expert:
            app.invoke_script(pfrm, "/scripts/config/hard/score.lisp");
            break;
        }

        return scene_pool::alloc<WorldMapScene>();
    }



    return null_scene();
}



} // namespace skyland
