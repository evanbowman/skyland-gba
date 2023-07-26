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


#include "fadeInScene.hpp"
#include "platform/color.hpp"
#include "readyScene.hpp"
#include "scriptHookScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void FadeInScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    WorldScene::notransitions();
    WorldScene::disable_ui();

    auto st = calc_screen_tiles(pfrm);
    for (int i = 1; i < st.x; ++i) {
        for (int j = 0; j < st.y; ++j) {
            pfrm.set_tile(Layer::overlay, i, j, 112);
        }
    }

    for (int y = 0; y < st.y; y += 3) {
        pfrm.set_tile(Layer::overlay, 0, y, 157);
        pfrm.set_tile(Layer::overlay, 0, y + 1, 112);
        pfrm.set_tile(Layer::overlay, 0, y + 2, 158);
    }

    pfrm.screen().set_shader(app.environment().shader(app));
}



void FadeInScene::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.set_scroll(Layer::overlay, 0, 0);
    WorldScene::exit(pfrm, app, next);

    pfrm.fill_overlay(0);
    scroll_amount_ = 0;
}



ScenePtr<Scene>
FadeInScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    timer_ += delta;

    app.environment().update(pfrm, app, delta);


    constexpr auto fade_duration = milliseconds(800);
    if (timer_ > fade_duration) {

        if (app.game_mode() == App::GameMode::sandbox or
            app.game_mode() == App::GameMode::adventure) {
            app.time_stream().enable_pushes(true);
            app.time_stream().clear();

            time_stream::event::Initial e;
            app.time_stream().push(app.level_timer(), e);
        }

        const auto loc = app.current_world_location();
        auto& node = app.world_graph().nodes_[loc];
        if (node.type_ == WorldGraph::Node::Type::corrupted) {
            if (not pfrm.speaker().is_music_playing(
                    app.environment().music())) {
                pfrm.speaker().play_music(app.environment().music(), 0);
            }
        }

        state_bit_store(app, StateBit::disable_autopause, false);

        pfrm.screen().fade(0.f);
        auto future_scene = [&pfrm, &app]() {
            auto next = scene_pool::alloc<ReadyScene>();
            if (app.game_mode() not_eq App::GameMode::tutorial and
                not pfrm.network_peer().is_connected()) {

                const auto loc = app.current_world_location();
                auto& node = app.world_graph().nodes_[loc];

                if (node.type_ == WorldGraph::Node::Type::hostile or
                    node.type_ == WorldGraph::Node::Type::hostile_hidden or
                    node.type_ == WorldGraph::Node::Type::exit or
                    app.game_mode() == App::GameMode::challenge) {

                    app.on_timeout(
                        pfrm, milliseconds(250), [](Platform& pfrm, App& app) {
                            if (auto w = app.scene().cast_world_scene()) {
                                if (state_bit_load(
                                        app, StateBit::disable_autopause)) {
                                    return;
                                }
                                w->set_gamespeed(pfrm, app, GameSpeed::stopped);
                            }
                        });
                }
            }
            app.dropped_frames_ = 0;
            return next;
        };

        if (app.opponent_island() and
            app.opponent_island()->get_drift() == 0.0_fixed) {
            // Bugfix: converge callback never fires because islands are both
            // really large.
            app.on_timeout(
                pfrm, milliseconds(500), [](Platform& pfrm, App& app) {
                    invoke_hook(pfrm, app, "on-converge");
                    lisp::set_var("on-converge", L_NIL);
                });
        }

        return scene_pool::alloc<ScriptHookScene>("on-fadein", future_scene);
    } else {
        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        // const auto amount2 = 1.f - smoothstep(0.f, (fade_duration / 4) * 3, timer_);
        const Color input(ColorConstant::rich_black);

        int scroll = pfrm.screen().size().x * amount;
        auto st = calc_screen_tiles(pfrm);
        for (int x = 0; x < ((int)pfrm.screen().size().x / 8) - scroll / 8;
             ++x) {
            for (int y = 0; y < 32; ++y) {
                auto prev = pfrm.get_tile(Layer::overlay, st.x - 1 - x, y);
                if (prev) {
                    pfrm.set_tile(Layer::overlay, st.x - 1 - x, y, 0);
                }
            }
        }
        scroll_amount_ = -scroll - 8;

        u8 amt = 255 - amount * 255;

        const Color ao(app.environment().fadein_colorize_tone());

        Color color(fast_interpolate(ao.r_, input.r_, amt),
                    fast_interpolate(ao.g_, input.g_, amt),
                    fast_interpolate(ao.b_, input.b_, amt));
        pfrm.screen().schedule_fade(amount, color.hex());
    }

    return null_scene();
}



void FadeInScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);
    pfrm.set_scroll(Layer::overlay, -scroll_amount_, 0);
}



} // namespace skyland
