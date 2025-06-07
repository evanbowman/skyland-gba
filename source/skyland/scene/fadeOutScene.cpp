////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "fadeOutScene.hpp"
#include "levelExitScene.hpp"
#include "selectChallengeScene.hpp"
#include "selectTutorialScene.hpp"
#include "skyland/player/autopilotPlayer.hpp"
#include "skyland/scene/modules/regressionModule.hpp"
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



ScenePtr FadeOutScene::update(Time delta)
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

        if (APP.opponent_island()) {
            for (auto& room : APP.opponent_island()->rooms()) {
                room->detach_drone(true);
            }
            APP.opponent_island()->drones().clear();
        }

        APP.player_island().drones().clear();

        for (auto& room : APP.player_island().rooms()) {
            room->unset_target();
        }


        PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::square_1);
        PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::square_2);
        PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::noise);
        PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::wave);

        APP.player_island().set_hidden(false);
        APP.player_island().set_phase(0);

        if (APP.opponent_island()) {
            APP.opponent_island()->set_hidden(false);
            APP.opponent_island()->set_phase(0);
        }

        hide_translucence();

        PLATFORM.screen().set_shader(passthrough_shader);
        PLATFORM.screen().fade(1.f);

        if (state_bit_load(StateBit::regression)) {
            return make_scene<LevelExitScene<RegressionModule>>();
        }

        switch (APP.game_mode()) {
        case App::GameMode::tutorial:
            return make_scene<LevelExitScene<SelectTutorialScene>>();

        case App::GameMode::adventure:
            return make_scene<LevelExitScene<ZoneImageScene>>();

        case App::GameMode::challenge:
            return make_scene<LevelExitScene<SelectChallengeScene>>();

        case App::GameMode::skyland_forever:
        case App::GameMode::sandbox:
            return make_scene<LevelExitScene<TitleScreenScene>>(3);

        case App::GameMode::co_op:
        case App::GameMode::multiplayer:
            return make_scene<LevelExitScene<TitleScreenScene>>();

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
