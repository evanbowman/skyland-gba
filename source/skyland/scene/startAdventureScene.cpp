////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "startAdventureScene.hpp"
#include "adventureLogScene.hpp"
#include "adventureModeSettingsScene.hpp"
#include "globals.hpp"
#include "menuPromptScene.hpp"
#include "readyScene.hpp"
#include "script/lisp.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/workshop.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



void load_difficulty_profile()
{
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
}



void StartAdventureScene::enter(Scene& prev)
{
    show_island_exterior(&APP.player_island());

    APP.with_opponent_island([](auto& isle) { show_island_exterior(&isle); });

    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    if (save::load("", APP.persistent_data())) {
        if (APP.persistent_data().state_flags_.get() &
            PersistentData::permadeath_on) {
            save::erase();
        }

        loaded_ = true;

        load_difficulty_profile();

    } else {
        reset_state();
    }

    if (not(APP.persistent_data().state_flags_.get() &
            PersistentData::permadeath_on) and
        loaded_) {

        PLATFORM.screen().schedule_fade(0, ColorConstant::rich_black);
        PLATFORM.screen().schedule_fade(1.f, ColorConstant::rich_black);

        Text::print(SYSTR(newgame)->c_str(), {3, 4});
        Text::print(SYSTR(continue_game)->c_str(), {3, 2});
    }
}



ScenePtr StartAdventureScene::update(Time delta)
{
    if (not(APP.persistent_data().state_flags_.get() &
            PersistentData::permadeath_on) and
        loaded_) {

        if (continue_opt_sel_ == 0) {
            PLATFORM.set_tile(Layer::overlay, 1, 2, 475);
            PLATFORM.set_tile(Layer::overlay, 1, 4, 112);
        } else {
            PLATFORM.set_tile(Layer::overlay, 1, 2, 112);
            PLATFORM.set_tile(Layer::overlay, 1, 4, 475);
        }

        if (key_down<Key::up>()) {
            if (continue_opt_sel_ == 1) {
                continue_opt_sel_ = 0;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }
        if (key_down<Key::down>()) {
            if (continue_opt_sel_ == 0) {
                continue_opt_sel_ = 1;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }

        if (key_down<Key::action_1>()) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
            if (continue_opt_sel_ == 1) {
                loaded_ = false;
                reset_state();
            }
            PLATFORM.fill_overlay(0);
        } else {
            return null_scene();
        }
    }

    auto& cursor_loc = globals().near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    APP.player_island().set_position(
        {Fixnum::from_integer(10), Fixnum::from_integer(374)});

    const auto sv_flag = GlobalPersistentData::save_prompt_dont_remind_me;

    const bool skip_save_prompt =
        APP.gp_.stateflags_.get(sv_flag) or
        (not(APP.persistent_data().state_flags_.get() &
             PersistentData::permadeath_on));

    auto dont_remind = []() {
        APP.gp_.stateflags_.set(sv_flag, true);
        save::store_global_data(APP.gp_);
    };

    DeferredScene next([] {
        auto ret = make_scene<AdventureLogScene>();
        ret->set_next_scene([] {
            auto scn = make_scene<ZoneImageScene>();
            scn->reset_nav_path_ = false;
            return scn;
        });
        return ret;
    });

    if (not loaded_) {
        next = [] {
            WorldMapScene::reset_nav_path();
            auto ret = make_scene<AdventureModeSettingsScene>(true);
            return ret;
        };
    }


    if (loaded_ and not skip_save_prompt) {

        auto ret = make_scene<MenuPromptScene>(
            SystemString::save_prompt,
            SystemString::ok,
            SystemString::do_not_show_again,
            next,
            []() {},
            dont_remind);

        ret->play_alert_sfx_ = false;
        ret->skip_unfade_ = true;
        return ret;

    } else {
        return next();
    }
}



void StartAdventureScene::reset_state()
{
    APP.set_coins(0);

    Character::__reset_ids();

    APP.current_world_location() = 0;
    APP.world_graph().generate();
    APP.persistent_data().lives_ = 2;

    APP.zone() = 1;

    APP.persistent_data().total_seconds_.set(0);
    APP.persistent_data().score_.set(0);
    APP.persistent_data().state_flags_.set(0);

    if (APP.is_developer_mode()) {
        APP.persistent_data().set_flag(
            PersistentData::StateFlag::dev_mode_active);
    }

    lisp::set_var("adventure-log", L_NIL);
}



} // namespace skyland
