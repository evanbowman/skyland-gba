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


#include "newgameScene.hpp"
#include "adventureLogScene.hpp"
#include "globals.hpp"
#include "loadLevelScene.hpp"
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



void NewgameScene::enter(Scene& prev)
{
    show_island_exterior(&APP.player_island());

    APP.with_opponent_island([](auto& isle) { show_island_exterior(&isle); });

    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

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

    cached_rng_ = rng::critical_state;

    if (save::load(APP.persistent_data())) {

        loaded_ = true;

    } else {
        reset_state();
    }

    if (loaded_) {

        PLATFORM.screen().schedule_fade(0, ColorConstant::rich_black);
        PLATFORM.screen().schedule_fade(1.f, ColorConstant::rich_black);

        Text::print(SYSTR(newgame)->c_str(), {3, 4});
        Text::print(SYSTR(continue_game)->c_str(), {3, 2});
    }
}



ScenePtr NewgameScene::update(Time delta)
{
    if (loaded_) {

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
        } else {
            return null_scene();
        }
    }

    auto& cursor_loc = globals().near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    APP.player_island().set_position(
        {Fixnum::from_integer(10), Fixnum::from_integer(374)});

    DeferredScene next([] {
        auto ret = make_scene<AdventureLogScene>();
        ret->set_next_scene([]() -> ScenePtr {
            if (APP.persistent_data().state_flags_.get() &
                PersistentData::entering_level) {
                APP.persistent_data().clear_flag(
                    PersistentData::entering_level);
                return make_scene<LoadLevelScene>();
            } else {
                return make_scene<ZoneImageScene>();
            }
        });
        return ret;
    });

    return next();
}



void NewgameScene::reset_state()
{
    rng::critical_state = cached_rng_;

    APP.set_coins(0);

    BasicCharacter::__reset_ids();

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
