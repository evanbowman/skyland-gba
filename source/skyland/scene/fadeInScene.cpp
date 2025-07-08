////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "fadeInScene.hpp"
#include "constructionScene.hpp"
#include "platform/color.hpp"
#include "readyScene.hpp"
#include "script/lisp.hpp"
#include "scriptHookScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"


namespace skyland
{

SHARED_VARIABLE(rewind_disabled);


void generate_snow(Island& isle)
{
    for (u8 x = 0; x < (int)isle.terrain().size(); ++x) {
        bool empty_column = true;
        u8 y;
        for (y = construction_zone_min_y; y < 14; ++y) {
            if (isle.rooms_plot().get(x, y)) {
                if (auto room = isle.get_room({x, y})) {
                    if ((*room->metaclass())->category() ==
                            Room::Category::decoration or
                        str_eq(room->name(), "drone-bay")) {
                        empty_column = false;
                        y -= 1;
                        break;
                    }
                    y -= 1;
                    break;
                }
            }
        }

        if (empty_column and y > construction_zone_min_y) {
            require_metaclass("snow")->create(&isle, {x, y});
        }
    }
}



void FadeInScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    WorldScene::notransitions();
    WorldScene::disable_ui();

    if (APP.environment().is_cold()) {
        if (APP.opponent_island()) {
            // generate_snow(*APP.opponent_island());
        }
    }

    auto st = calc_screen_tiles();
    for (int i = 1; i < st.x; ++i) {
        for (int j = 0; j < st.y; ++j) {
            PLATFORM.set_tile(Layer::overlay, i, j, 112);
        }
    }

    for (int y = 0; y < st.y; y += 3) {
        PLATFORM.set_tile(Layer::overlay, 0, y, 157);
        PLATFORM.set_tile(Layer::overlay, 0, y + 1, 112);
        PLATFORM.set_tile(Layer::overlay, 0, y + 2, 158);
    }

    PLATFORM.screen().set_shader(APP.environment().shader());

    APP.player_island().schedule_repaint();

    if (APP.opponent_island()) {
        APP.opponent_island()->schedule_repaint();
    }
}



void FadeInScene::exit(Scene& next)
{
    PLATFORM.set_scroll(Layer::overlay, 0, 0);
    WorldScene::exit(next);

    PLATFORM.fill_overlay(0);
    scroll_amount_ = 0;
}



ScenePtr FadeInScene::update(Time delta)
{
    WorldScene::update(delta);

    timer_ += delta;

    APP.environment().update(delta);


    constexpr auto fade_duration = milliseconds(800);
    if (timer_ > fade_duration) {

        if (APP.game_mode() == App::GameMode::sandbox or
            APP.game_mode() == App::GameMode::adventure) {
            APP.time_stream().enable_pushes(true);
            APP.time_stream().clear();

            time_stream::event::Initial e;
            APP.push_time_stream(e);
        }

        const auto loc = APP.current_world_location();
        auto& node = APP.world_graph().nodes_[loc];
        if (node.type_ == WorldGraph::Node::Type::corrupted) {
            if (not PLATFORM.speaker().is_music_playing(
                    APP.environment().music()->c_str())) {
                PLATFORM.speaker().stream_music(
                    APP.environment().music()->c_str(), 0);
            }
        }

        if (rewind_disabled) {
            APP.time_stream().enable_pushes(false);
        }

        state_bit_store(StateBit::disable_autopause, false);

        PLATFORM.screen().fade(0.f);
        auto future_scene = []() {
            auto next = make_scene<ReadyScene>();
            if (APP.game_mode() not_eq App::GameMode::tutorial and
                not PLATFORM.network_peer().is_connected()) {

                const auto loc = APP.current_world_location();
                auto& node = APP.world_graph().nodes_[loc];

                if (node.type_ == WorldGraph::Node::Type::hostile or
                    node.type_ == WorldGraph::Node::Type::hostile_hidden or
                    node.type_ == WorldGraph::Node::Type::exit or
                    APP.game_mode() == App::GameMode::challenge) {

                    APP.on_timeout(milliseconds(250), []() {
                        if (auto w = APP.scene().cast_world_scene()) {
                            if (state_bit_load(StateBit::disable_autopause)) {
                                return;
                            }
                            w->set_gamespeed(GameSpeed::stopped);
                        }
                    });
                }
            }
            APP.dropped_frames_ = 0;
            return next;
        };

        if (APP.opponent_island() and
            APP.opponent_island()->get_drift() == 0.0_fixed) {
            // Bugfix: converge callback never fires because islands are both
            // really large.
            APP.on_timeout(milliseconds(500), []() {
                invoke_hook("on-converge");
                lisp::set_var("on-converge", L_NIL);
            });
        }

        return make_scene<ScriptHookScene>("on-fadein", future_scene);
    } else {
        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        // const auto amount2 = 1.f - smoothstep(0.f, (fade_duration / 4) * 3, timer_);
        const Color input(ColorConstant::rich_black);

        int scroll = PLATFORM.screen().size().x * amount;
        auto st = calc_screen_tiles();
        for (int x = 0; x < ((int)PLATFORM.screen().size().x / 8) - scroll / 8;
             ++x) {
            for (int y = 0; y < 32; ++y) {
                auto prev = PLATFORM.get_tile(Layer::overlay, st.x - 1 - x, y);
                if (prev) {
                    PLATFORM.set_tile(Layer::overlay, st.x - 1 - x, y, 0);
                }
            }
        }
        scroll_amount_ = -scroll - 8;

        u8 amt = 255 - amount * 255;

        const Color ao(APP.environment().fadein_colorize_tone());

        Color color(fast_interpolate(ao.r_, input.r_, amt),
                    fast_interpolate(ao.g_, input.g_, amt),
                    fast_interpolate(ao.b_, input.b_, amt));
        PLATFORM.screen().schedule_fade(amount, {.color = color.hex()});
    }

    return null_scene();
}



void FadeInScene::display()
{
    WorldScene::display();
    PLATFORM.set_scroll(Layer::overlay, -scroll_amount_, 0);
}



} // namespace skyland
