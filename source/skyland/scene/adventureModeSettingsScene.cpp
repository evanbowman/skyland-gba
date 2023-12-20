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


#include "adventureModeSettingsScene.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland
{



void AdventureModeSettingsScene::enter(Scene& prev)
{
    auto title = SYSTR(sf_difficulty);

    difficulty_text_.emplace(
        title->c_str(),
        OverlayCoord{(u8)centered_text_margins(strlen(title->c_str())), 1});


    auto str1 = SYSTR(sf_casual);
    easy_text_.emplace(
        str1->c_str(),
        OverlayCoord{(u8)centered_text_margins(strlen(str1->c_str())), 4});


    auto str2 = SYSTR(sf_normal);
    normal_text_.emplace(
        str2->c_str(),
        OverlayCoord{(u8)centered_text_margins(strlen(str2->c_str())), 6});


    auto str3 = SYSTR(sf_hard);
    hard_text_.emplace(
        str3->c_str(),
        OverlayCoord{(u8)centered_text_margins(strlen(str3->c_str())), 8});


    PLATFORM.screen().fade(0.96f);
    PLATFORM.screen().fade(1.f);

    original_ = (u8)APP.gp_.difficulty_;

    desc_.emplace();
}



void AdventureModeSettingsScene::exit(Scene& prev)
{
    difficulty_text_.reset();
    easy_text_.reset();
    normal_text_.reset();
    hard_text_.reset();
    desc_.reset();
}



ScenePtr<Scene> AdventureModeSettingsScene::update(Time delta)
{
    bool repaint = false;

    if (init_) {
        init_ = false;
        repaint = true;
    }

    if (APP.player().key_down(Key::up)) {
        auto& diff = APP.gp_.difficulty_;
        if ((int)diff == 0) {
            diff = GlobalPersistentData::Difficulty::expert;
        } else {
            diff = (GlobalPersistentData::Difficulty)(((u8)diff - 1) % 3);
        }

        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint = true;
    }

    if (APP.player().key_down(Key::down)) {
        auto& diff = APP.gp_.difficulty_;
        diff = (GlobalPersistentData::Difficulty)(((u8)diff + 1) % 3);
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint = true;
    }

    auto sel = [](auto& text, int tile) {
        PLATFORM.set_tile(
            Layer::overlay, text->coord().x - 2, text->coord().y, tile);
    };

    if (repaint) {

        auto hint_colors = Text::OptColors{
            {ColorConstant::med_blue_gray, ColorConstant::rich_black}};

        switch (APP.gp_.difficulty_) {
        case GlobalPersistentData::Difficulty::beginner:
            sel(easy_text_, 396);
            sel(normal_text_, 0);
            sel(hard_text_, 0);
            desc_->assign(SYS_CSTR(difficulty_hint_easy),
                          {1, 14},
                          {28, 6},
                          0,
                          hint_colors);
            break;

        case GlobalPersistentData::Difficulty::experienced:
            sel(easy_text_, 0);
            sel(normal_text_, 396);
            sel(hard_text_, 0);
            desc_->assign(SYS_CSTR(difficulty_hint_normal),
                          {1, 14},
                          {28, 6},
                          0,
                          hint_colors);
            break;

        case GlobalPersistentData::Difficulty::expert:
            sel(easy_text_, 0);
            sel(normal_text_, 0);
            sel(hard_text_, 396);
            desc_->assign(SYS_CSTR(difficulty_hint_hard),
                          {1, 14},
                          {28, 6},
                          0,
                          hint_colors);
            break;
        }
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
