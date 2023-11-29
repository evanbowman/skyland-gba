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



void FadeOutScene::enter(Scene& prev)
{
    WorldScene::enter(prev);
    disable_ui();
}



void FadeOutScene::exit(Scene& prev)
{
    WorldScene::exit(prev);
}



ScenePtr<Scene> FadeOutScene::update(Microseconds delta)
{
    WorldScene::update(delta);

    timer_ += delta;

    APP.time_stream().enable_pushes(false);
    APP.time_stream().clear();

    constexpr auto fade_duration = milliseconds(800);

    if (timer_ > fade_duration) {

        circ_effect_radius_ = 0;

        PLATFORM.screen().clear();
        display();
        PLATFORM.sleep(5);

        for (auto& room : APP.player_island().rooms()) {
            room->detach_drone(true);
        }

        for (auto& room : APP.opponent_island()->rooms()) {
            room->detach_drone(true);
        }

        APP.player_island().drones().clear();
        APP.opponent_island()->drones().clear();

        for (auto& room : APP.player_island().rooms()) {
            room->unset_target();
        }


        PLATFORM.speaker().stop_chiptune_note(
            Platform::Speaker::Channel::square_1);
        PLATFORM.speaker().stop_chiptune_note(
            Platform::Speaker::Channel::square_2);
        PLATFORM.speaker().stop_chiptune_note(
            Platform::Speaker::Channel::noise);
        PLATFORM.speaker().stop_chiptune_note(Platform::Speaker::Channel::wave);

        APP.player_island().set_hidden(false);
        if (APP.opponent_island()) {
            APP.opponent_island()->set_hidden(false);
        }

        PLATFORM.screen().set_shader(passthrough_shader);
        PLATFORM.screen().fade(1.f);
        switch (APP.game_mode()) {
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

        case App::GameMode::macro:
            Platform::fatal("logic error: macro fadeout!?");
        }
    } else {
        const auto amount = smoothstep(0.f, fade_duration, timer_);
        PLATFORM.screen().schedule_fade(amount);
        circ_effect_radius_ = 144 - int(144 * amount);
    }

    return null_scene();
}



void FadeOutScene::display()
{
    WorldScene::display();

    // int circ_center_x = PLATFORM.screen().size().x / 2;
    // int circ_center_y = PLATFORM.screen().size().y / 2;

    // Platform::fatal(stringify(circ_center_y).c_str());
    // int params[] = {circ_effect_radius_, circ_center_x, circ_center_y};
    // PLATFORM.system_call("iris-wipe-effect", params);
}



} // namespace skyland
