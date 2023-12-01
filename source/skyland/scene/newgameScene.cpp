////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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



ScenePtr<Scene> NewgameScene::update(Microseconds delta)
{
    show_island_exterior(&APP.player_island());
    show_island_exterior(APP.opponent_island());

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

    bool loaded = false;

    if (save::load(APP.persistent_data())) {
        save::erase();
        loaded = true;
    } else {
        APP.set_coins(0);

        BasicCharacter::__reset_ids();

        APP.current_world_location() = 0;
        APP.world_graph().generate();
        APP.persistent_data().lives_ = 2;

        APP.zone() = 1;

        APP.persistent_data().total_pauses_.set(0);
        APP.persistent_data().total_seconds_.set(0);
        APP.persistent_data().score_.set(0);
        APP.persistent_data().state_flags_.set(0);

        if (APP.is_developer_mode()) {
            APP.persistent_data().set_flag(
                PersistentData::StateFlag::dev_mode_active);
        }

        lisp::set_var("adventure-log", L_NIL);
    }

    auto& cursor_loc = globals().near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    APP.player_island().set_position(
        {Fixnum::from_integer(10), Fixnum::from_integer(374)});

    const auto sv_flag = GlobalPersistentData::save_prompt_dont_remind_me;

    const bool skip_save_prompt = APP.gp_.stateflags_.get(sv_flag);

    auto dont_remind = []() {
        APP.gp_.stateflags_.set(sv_flag, true);
        save::store_global_data(APP.gp_);
    };

    DeferredScene next([] {
        auto ret = scene_pool::alloc<AdventureLogScene>();
        ret->set_next_scene([] { return scene_pool::alloc<ZoneImageScene>(); });
        return ret;
    });


    if (loaded and not skip_save_prompt) {

        auto ret = scene_pool::alloc<MenuPromptScene>(
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



} // namespace skyland
