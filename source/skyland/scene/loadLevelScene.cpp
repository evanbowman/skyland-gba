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


#include "loadLevelScene.hpp"
#include "fadeInScene.hpp"
#include "fullscreenDialogScene.hpp"
#include "globals.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/blizzard.hpp"
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
        {Fixnum(Float(250 + 16 * (10 - right_island.terrain().size()))),
         Fixnum::from_integer(374)});
}



void LoadLevelScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    app.player_island().render(pfrm, app);
}



void LoadLevelScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    GenericBird::generate(pfrm, app);
}



SHARED_VARIABLE(zone1_coin_yield);
SHARED_VARIABLE(zone2_coin_yield);
SHARED_VARIABLE(zone3_coin_yield);
SHARED_VARIABLE(zone4_coin_yield);



void prep_level(Platform& pfrm, App& app)
{
    auto& cursor_loc = globals().near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;
    auto& far_cursor_loc = globals().far_cursor_loc_;
    far_cursor_loc.x = 0;
    far_cursor_loc.y = 14;


    app.dropped_frames_ = 0;
    app.victory_coins() = 0;
    app.pause_count() = 0;
    app.stat_timer().reset(0);
    app.level_timer().reset(0);
    state_bit_store(app, StateBit::surrender_offered, false);
    rng::get(app.crane_game_rng());
    state_bit_store(app, StateBit::crane_game_got_treasure, false);

    if (app.game_mode() not_eq App::GameMode::skyland_forever and
        app.game_mode() not_eq App::GameMode::co_op) {
        app.player().rooms_built_ = 0;
        app.player().rooms_lost_ = 0;
    }

    app.persistent_data().score_.set(
        std::max((s32)0, app.persistent_data().score_.get()));


    app.level_begin_score() = app.persistent_data().score_.get();

    app.player_island().init_ai_awareness(pfrm, app);


    if (app.opponent_island()) {
        app.opponent_island()->set_drift(pfrm, app, Fixnum(-0.000025f));

        set_island_positions(app.player_island(), *app.opponent_island());

        app.player_island().set_float_timer(0);

        app.opponent_island()->set_float_timer(
            std::numeric_limits<Microseconds>::max() / 2);

        for (auto& room : app.opponent_island()->rooms()) {
            if (str_eq(room->name(), "mycelium")) {
                // The construction cost of the initial block is high, and we
                // don't want to award the player a large amount of coins for
                // each block spawned.
                app.victory_coins() += 100;
            } else if (app.zone() < 2) {
                app.victory_coins() +=
                    (0.01f * zone1_coin_yield) * (*room->metaclass())->cost();
            } else if (app.zone() < 3) {
                app.victory_coins() +=
                    (0.01f * zone2_coin_yield) * (*room->metaclass())->cost();
            } else if (app.zone() < 4) {
                app.victory_coins() +=
                    (0.01f * zone3_coin_yield) * (*room->metaclass())->cost();
            } else {
                app.victory_coins() +=
                    (0.01f * zone4_coin_yield) * (*room->metaclass())->cost();
            }
        }

        app.opponent_island()->init_ai_awareness(pfrm, app);


        show_island_exterior(pfrm, app, app.opponent_island());


        write_custom_graphics(pfrm, app);
        app.opponent_island()->render_exterior(pfrm, app);
    }
}



ScenePtr<Scene>
LoadLevelScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    const auto loc = app.current_world_location();
    info(pfrm, format("%", loc));
    auto& node = app.world_graph().nodes_[loc];

    for (auto& room : app.player_island().rooms()) {
        room->detach_drone(pfrm, app, true);
    }
    app.player_island().drones().clear();

    if (app.zone() > 3) {
        app.swap_environment<weather::Blizzard>();
    } else if (app.zone() > 2) {
        app.swap_environment<weather::Storm>();
    } else {
        app.swap_environment<weather::ClearSkies>();
    }

    switch (node.type_) {
    case WorldGraph::Node::Type::neutral:
    case WorldGraph::Node::Type::neutral_hidden:
    default: {
        app.invoke_script(pfrm, "/scripts/event/neutral.lisp");
        break;
    }

    case WorldGraph::Node::Type::quest:
        app.invoke_script(pfrm, "/scripts/event/quest.lisp");
        break;

    case WorldGraph::Node::Type::corrupted:
        app.invoke_script(pfrm, "/scripts/event/storm_king.lisp");
        break;

    case WorldGraph::Node::Type::quest_marker:
        app.invoke_script(pfrm, "/scripts/event/quest_marker.lisp");
        break;

    case WorldGraph::Node::Type::exit:
    case WorldGraph::Node::Type::hostile:
    case WorldGraph::Node::Type::hostile_hidden: {
        app.invoke_script(pfrm, "/scripts/event/hostile.lisp");
        break;
    }
    }

    if (node.type_ == WorldGraph::Node::Type::corrupted) {
        pfrm.speaker().play_music("unaccompanied_wind", 0);

    } else {
        if (not pfrm.speaker().is_music_playing(app.environment().music())) {
            pfrm.speaker().play_music(app.environment().music(), 0);
        }
    }

    prep_level(pfrm, app);

    for (auto& room : app.player_island().rooms()) {
        room->reset_state();
    }

    if (app.dialog_buffer()) {
        auto buffer = std::move(*app.dialog_buffer());
        app.dialog_buffer().reset();

        auto future_scene = [&pfrm,
                             &app,
                             storm_king = node.type_ ==
                                          WorldGraph::Node::Type::corrupted] {
            if (storm_king) {
                app.swap_environment<weather::Typhoon>();
            }

            return scene_pool::alloc<FadeInScene>();
        };
        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
                                                        future_scene);
    }

    return scene_pool::alloc<FadeInScene>();
}



} // namespace skyland
