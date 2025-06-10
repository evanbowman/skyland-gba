////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "loadLevelScene.hpp"
#include "fadeInScene.hpp"
#include "fullscreenDialogScene.hpp"
#include "globals.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/minimap.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/weatherEngine.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/blizzard.hpp"
#include "skyland/weather/slightlyOvercast.hpp"
#include "skyland/weather/typhoon.hpp"



namespace skyland
{



void set_island_positions(Island& left_island, Island& right_island)
{
    // Now, you may be wondering, why did I put the player and opponent islands
    // at strange y-positions like 374? I forked one of my other GBA projects
    // when developing the jam version of SKYLAND. The engine from BlindJump
    // used larger background maps, so a y of 300 was more centrally located. In
    // any event, the starting y position will always be some weird constant no
    // matter what I do. I suppose I could have started the number at zero, but
    // I didn't know how big the islands were going to be originally, so I gave
    // myself extra space to work with.

    left_island.set_position(
        {Fixnum::from_integer(10), Fixnum::from_integer(374)});
    // Pretty much as far away as possible, without wrapping across the screen.
    right_island.set_position(
        {Fixnum::from_integer(250 + 16 * (10 - right_island.terrain().size())),
         Fixnum::from_integer(374)});
}



void LoadLevelScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    APP.player_island().render();
}



void LoadLevelScene::exit(Scene& next)
{
    WorldScene::exit(next);

    GenericBird::generate();
}



SHARED_VARIABLE(zone1_coin_yield);
SHARED_VARIABLE(zone2_coin_yield);
SHARED_VARIABLE(zone3_coin_yield);
SHARED_VARIABLE(zone4_coin_yield);



void prep_level()
{
    for (int x = 0; x < minimap::player_destroyed_rooms.size().x; ++x) {
        for (int y = 0; y < minimap::player_destroyed_rooms.size().y; ++y) {
            minimap::player_destroyed_rooms.set(x, y, false);
        }
    }

    auto& cursor_loc = globals().near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;
    auto& far_cursor_loc = globals().far_cursor_loc_;
    far_cursor_loc.x = 0;
    far_cursor_loc.y = 14;

    APP.player_island().projectiles().clear();
    APP.player_island().set_phase(0);

    const auto pl_terrain_size = APP.player_island().terrain().size();
    if (pl_terrain_size > 8) {
        cursor_loc.x = pl_terrain_size / 2;
    }

    for (auto& room : APP.player_island().rooms()) {
        room->update_description();
        room->on_level_start();
    }

    // Bugfix: in case an island is destroyed by a projectile after processing a
    // lisp exit call. e.g. island surrenders then hit by a missile.
    APP.exit_condition() = App::ExitCondition::none;

    APP.dropped_frames_ = 0;
    APP.victory_coins() = 0;
    APP.pause_count() = 0;
    APP.stat_timer().reset(0);
    APP.level_timer().reset(0);
    state_bit_store(StateBit::surrender_offered, false);
    state_bit_store(StateBit::crane_game_got_treasure, false);

    if (APP.game_mode() not_eq App::GameMode::skyland_forever and
        APP.game_mode() not_eq App::GameMode::co_op) {
        APP.player().rooms_built_ = 0;
        APP.player().rooms_lost_ = 0;
    }

    APP.player().on_level_start();
    APP.opponent().on_level_start();

    APP.persistent_data().score_.set(
        std::max((s32)0, APP.persistent_data().score_.get()));


    APP.level_begin_score() = APP.persistent_data().score_.get();

    APP.persistent_data().lives_ = 2;

    APP.player_island().init_ai_awareness();
    APP.player_island().fires_extinguish();

    if (APP.opponent_island()) {
        APP.opponent_island()->set_drift(Fixnum(-0.000025f));

        set_island_positions(APP.player_island(), *APP.opponent_island());

        APP.player_island().set_float_timer(0);

        APP.opponent_island()->set_float_timer(
            std::numeric_limits<Time>::max() / 2);

        for (auto& room : APP.opponent_island()->rooms()) {
            if (str_eq(room->name(), "mycelium")) {
                // The construction cost of the initial block is high, and we
                // don't want to award the player a large amount of coins for
                // each block spawned.
                APP.victory_coins() += 100;
            } else if (APP.zone() < 2) {
                APP.victory_coins() +=
                    (0.01f * zone1_coin_yield) * (*room->metaclass())->cost();
            } else if (APP.zone() < 3) {
                APP.victory_coins() +=
                    (0.01f * zone2_coin_yield) * (*room->metaclass())->cost();
            } else if (APP.zone() < 4) {
                APP.victory_coins() +=
                    (0.01f * zone3_coin_yield) * (*room->metaclass())->cost();
            } else {
                APP.victory_coins() +=
                    (0.01f * zone4_coin_yield) * (*room->metaclass())->cost();
            }
        }

        APP.opponent_island()->init_ai_awareness();


        show_island_exterior(APP.opponent_island());


        write_custom_graphics();
    } else {
        APP.player_island().set_position(
            {Fixnum::from_integer(10), Fixnum::from_integer(374)});
    }
}



void update_weather_onload()
{
    bool has_weather_engine = false;
    for (auto& r : APP.player_island().rooms()) {
        if (r->cast<WeatherEngine>()) {
            has_weather_engine = true;
            break;
        }
    }

    if (has_weather_engine) {
        // Maintain current weather
    } else if (APP.zone() > 3) {
        APP.swap_environment<weather::Blizzard>();
    } else if (APP.zone() > 2) {
        APP.swap_environment<weather::Storm>();
    } else if (APP.zone() > 1) {
        APP.swap_environment<weather::SlightlyOvercast>();
    } else {
        APP.swap_environment<weather::ClearSkies>();
    }

    APP.player_island().recalculate_power_usage();
}



ScenePtr LoadLevelScene::update(Time delta)
{
    const auto loc = APP.current_world_location();
    auto& node = APP.world_graph().nodes_[loc];

    for (auto& room : APP.player_island().rooms()) {
        room->detach_drone(true);
    }
    APP.player_island().drones().clear();

    update_weather_onload();

    switch (node.type_) {
    case WorldGraph::Node::Type::neutral:
    case WorldGraph::Node::Type::neutral_hidden:
    default: {
        APP.invoke_script("/scripts/event/neutral.lisp");
        break;
    }

    case WorldGraph::Node::Type::quest:
        APP.invoke_script("/scripts/event/quest.lisp");
        break;

    case WorldGraph::Node::Type::corrupted:
        APP.invoke_script("/scripts/event/storm_king.lisp");
        break;

    case WorldGraph::Node::Type::quest_marker:
        APP.invoke_script("/scripts/event/quest_marker.lisp");
        break;

    case WorldGraph::Node::Type::hostile_hidden: {
        APP.invoke_script("/scripts/event/uncharted.lisp");
        break;
    }

    case WorldGraph::Node::Type::exit:
    case WorldGraph::Node::Type::hostile: {
        APP.invoke_script("/scripts/event/hostile.lisp");
        break;
    }
    case WorldGraph::Node::Type::shop:
        APP.invoke_script("/scripts/event/shop/shop.lisp");
        break;
    }

    // Eh... can't hurt to run it. Better than it running during the middle of a
    // level. At least the screen is currently black and people know that a
    // level is loading.
    lisp::gc();

    if (node.type_ == WorldGraph::Node::Type::corrupted) {
        PLATFORM.speaker().stream_music("unaccompanied_wind", 0);

    } else {
        if (not PLATFORM.speaker().is_music_playing(
                APP.environment().music()->c_str())) {
            PLATFORM.speaker().stream_music(APP.environment().music()->c_str(),
                                            0);
        }
    }

    prep_level();

    if (node.type_ == WorldGraph::Node::Type::shop) {
        APP.opponent_island()->set_position(
            {Fixnum((Float)APP.player_island().terrain().size() * 16 + 48),
             Fixnum(APP.opponent_island()->get_position().y)});
    }

    for (auto& room : APP.player_island().rooms()) {
        room->reset_state();
    }

    if (APP.dialog_buffer()) {
        auto buffer = std::move(*APP.dialog_buffer());
        APP.dialog_buffer().reset();

        auto future_scene = [storm_king = node.type_ ==
                                          WorldGraph::Node::Type::corrupted] {
            if (storm_king) {
                APP.swap_environment<weather::Typhoon>();
            }

            return make_scene<FadeInScene>();
        };
        return make_scene<FullscreenDialogScene>(std::move(buffer),
                                                 future_scene);
    }

    return make_scene<FadeInScene>();
}



} // namespace skyland
