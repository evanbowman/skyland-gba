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


    auto str1 = SYSTR(sf_casual);
    easy_text_.emplace(
        pfrm,
        str1->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(str1->c_str())),
                     4});


    auto str2 = SYSTR(sf_normal);
    normal_text_.emplace(
        pfrm,
        str2->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(str2->c_str())),
                     6});


    auto str3 = SYSTR(sf_hard);
    hard_text_.emplace(
        pfrm,
        str3->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(str3->c_str())),
                     8});

    pfrm.screen().fade(0.96f);
    pfrm.screen().fade(1.f);

    original_ = (u8)app.gp_.difficulty_;
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
        auto& diff = app.gp_.difficulty_;
        diff = (GlobalPersistentData::Difficulty)(((u8)diff - 1) % 3);
        pfrm.speaker().play_sound("click_wooden", 2);
    }

    if (app.player().key_down(pfrm, Key::down)) {
        auto& diff = app.gp_.difficulty_;
        diff = (GlobalPersistentData::Difficulty)(((u8)diff + 1) % 3);
        pfrm.speaker().play_sound("click_wooden", 2);
    }

    auto sel = [&pfrm](auto& text, int tile) {
        pfrm.set_tile(
            Layer::overlay, text->coord().x - 2, text->coord().y, tile);
    };

    switch (app.gp_.difficulty_) {
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

    if (app.player().key_down(pfrm, Key::action_1)) {
        pfrm.speaker().play_sound("button_wooden", 3);
        switch (app.gp_.difficulty_) {
        case GlobalPersistentData::Difficulty::beginner:
            app.invoke_script(pfrm, "/scripts/config/easy/score.lisp");
            break;

        case GlobalPersistentData::Difficulty::experienced:
            app.invoke_script(pfrm, "/scripts/config/normal/score.lisp");
            break;

        case GlobalPersistentData::Difficulty::expert:
            app.invoke_script(pfrm, "/scripts/config/hard/score.lisp");
            break;
        }

        if ((u8)app.gp_.difficulty_ not_eq original_) {
            save::store_global_data(pfrm, app.gp_);
        }

        if (newgame_) {
            // Hack to add easy-mode coin bonus to player inventory when
            // selecting initial difficulty.
            if (app.gp_.difficulty_ ==
                GlobalPersistentData::Difficulty::beginner) {
                app.set_coins(pfrm, app.coins() + 1500);
            }
        }

        return scene_pool::alloc<WorldMapScene>();
    }



    return null_scene();
}



} // namespace skyland
