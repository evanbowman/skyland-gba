////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "fadeOutScene.hpp"
#include "selectChallengeScene.hpp"
#include "selectTutorialScene.hpp"
#include "skyland/player/autopilotPlayer.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



ScenePtr<Scene>
FadeOutScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    timer_ += delta;

    app.time_stream().enable_pushes(false);
    app.time_stream().clear();

    constexpr auto fade_duration = milliseconds(800);
    if (timer_ > fade_duration) {


        for (auto& room : app.player_island().rooms()) {
            room->detach_drone(pfrm, app, true);
        }

        for (auto& room : app.opponent_island()->rooms()) {
            room->detach_drone(pfrm, app, true);
        }

        app.player_island().drones().clear();
        app.opponent_island()->drones().clear();

        for (auto& room : app.player_island().rooms()) {
            room->unset_target(pfrm, app);
        }


        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::square_1);
        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::square_2);
        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::noise);
        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::wave);

        app.player_island().set_hidden(pfrm, app, false);
        if (app.opponent_island()) {
            app.opponent_island()->set_hidden(pfrm, app, false);
        }

        pfrm.screen().set_shader(passthrough_shader);
        pfrm.screen().fade(1.f);
        switch (app.game_mode()) {
        case App::GameMode::tutorial:
            return scene_pool::alloc<SelectTutorialScene>();

        case App::GameMode::adventure:
            return scene_pool::alloc<ZoneImageScene>();

        case App::GameMode::challenge:
            return scene_pool::alloc<SelectChallengeScene>();

        case App::GameMode::skyland_forever:
        case App::GameMode::sandbox:
            return scene_pool::alloc<TitleScreenScene>(3);

        case App::GameMode::co_op:
        case App::GameMode::multiplayer:
            return scene_pool::alloc<TitleScreenScene>();
        }
    } else {
        const auto amount = smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().schedule_fade(amount);
    }

    return null_scene();
}



} // namespace skyland
