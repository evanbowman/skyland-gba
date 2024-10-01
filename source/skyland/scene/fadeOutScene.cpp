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


#include "fadeOutScene.hpp"
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

        if (state_bit_load(StateBit::regression)) {
            return make_scene<RegressionModule>();
        }

        switch (APP.game_mode()) {
        case App::GameMode::tutorial:
            return make_scene<SelectTutorialScene>();

        case App::GameMode::adventure:
            return make_scene<ZoneImageScene>();

        case App::GameMode::challenge:
            return make_scene<SelectChallengeScene>();

        case App::GameMode::skyland_forever:
        case App::GameMode::sandbox:
            return make_scene<TitleScreenScene>(3);

        case App::GameMode::co_op:
        case App::GameMode::multiplayer:
            return make_scene<TitleScreenScene>();

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
