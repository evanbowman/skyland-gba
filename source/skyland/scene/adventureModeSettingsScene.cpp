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



void AdventureModeSettingsScene::enter(Scene& prev)
{
    const char* difficulty_str = "difficulty:";

    difficulty_text_.emplace(

        difficulty_str,
        OverlayCoord{(u8)centered_text_margins(str_len(difficulty_str)), 1});


    auto str1 = SYSTR(sf_casual);
    easy_text_.emplace(

        str1->c_str(),
        OverlayCoord{(u8)centered_text_margins(str_len(str1->c_str())), 4});


    auto str2 = SYSTR(sf_normal);
    normal_text_.emplace(

        str2->c_str(),
        OverlayCoord{(u8)centered_text_margins(str_len(str2->c_str())), 6});


    auto str3 = SYSTR(sf_hard);
    hard_text_.emplace(

        str3->c_str(),
        OverlayCoord{(u8)centered_text_margins(str_len(str3->c_str())), 8});

    PLATFORM.screen().fade(0.96f);
    PLATFORM.screen().fade(1.f);

    original_ = (u8)APP.gp_.difficulty_;
}



void AdventureModeSettingsScene::exit(Scene& prev)
{
    difficulty_text_.reset();
    easy_text_.reset();
    normal_text_.reset();
    hard_text_.reset();
}



ScenePtr<Scene> AdventureModeSettingsScene::update(Microseconds delta)
{
    if (APP.player().key_down(Key::up)) {
        auto& diff = APP.gp_.difficulty_;
        diff = (GlobalPersistentData::Difficulty)(((u8)diff - 1) % 3);
        PLATFORM.speaker().play_sound("click_wooden", 2);
    }

    if (APP.player().key_down(Key::down)) {
        auto& diff = APP.gp_.difficulty_;
        diff = (GlobalPersistentData::Difficulty)(((u8)diff + 1) % 3);
        PLATFORM.speaker().play_sound("click_wooden", 2);
    }

    auto sel = [](auto& text, int tile) {
        PLATFORM.set_tile(
            Layer::overlay, text->coord().x - 2, text->coord().y, tile);
    };

    switch (APP.gp_.difficulty_) {
    case GlobalPersistentData::Difficulty::beginner:
        sel(easy_text_, 396);
        sel(normal_text_, 0);
        sel(hard_text_, 0);
        break;

    case GlobalPersistentData::Difficulty::experienced:
        sel(easy_text_, 0);
        sel(normal_text_, 396);
        sel(hard_text_, 0);
        break;

    case GlobalPersistentData::Difficulty::expert:
        sel(easy_text_, 0);
        sel(normal_text_, 0);
        sel(hard_text_, 396);
        break;
    }

    if (APP.player().key_down(Key::action_1)) {
        PLATFORM.speaker().play_sound("button_wooden", 3);
        switch (APP.gp_.difficulty_) {
        case GlobalPersistentData::Difficulty::beginner:
            APP.invoke_script("/scripts/config/easy/score.lisp");
            break;

        case GlobalPersistentData::Difficulty::experienced:
            APP.invoke_script("/scripts/config/normal/score.lisp");
            break;

        case GlobalPersistentData::Difficulty::expert:
            APP.invoke_script("/scripts/config/hard/score.lisp");
            break;
        }

        if ((u8)APP.gp_.difficulty_ not_eq original_) {
            save::store_global_data(APP.gp_);
        }

        if (newgame_) {
            APP.invoke_script("/scripts/newgame.lisp");
        }

        return scene_pool::alloc<WorldMapScene>();
    }



    return null_scene();
}



} // namespace skyland
