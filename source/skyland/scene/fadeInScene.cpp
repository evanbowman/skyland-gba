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

    pfrm.screen().set_shader(app.environment().shader(app));
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

        pfrm.screen().fade(0.f);
        auto future_scene = [&pfrm, &app]()
        {
            auto next = scene_pool::alloc<ReadyScene>();
            if (app.game_mode() not_eq App::GameMode::tutorial and
                not pfrm.network_peer().is_connected()) {

                const auto loc = app.current_world_location();
                auto& node = app.world_graph().nodes_[loc];

                if (node.type_ == WorldGraph::Node::Type::hostile or
                    node.type_ == WorldGraph::Node::Type::hostile_hidden or
                    app.game_mode() == App::GameMode::challenge) {
                    next->set_gamespeed(pfrm, app, GameSpeed::stopped);
                }
            }
            return next;
        };
        return scene_pool::alloc<ScriptHookScene>("on-fadein", future_scene);
    } else {
        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().schedule_fade(amount);
    }

    return null_scene();
}



} // namespace skyland
