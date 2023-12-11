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


#pragma once


#include "fadeInScene.hpp"
#include "skyland/network.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland
{



class MultiplayerReadyScene : public WorldScene
{
public:
    ScenePtr<Scene> update(Time delta) override
    {
        WorldScene::update(delta);

        timer_ += delta;

        switch (state_) {
        case State::fade_out: {
            constexpr auto fade_duration = milliseconds(800);
            if (timer_ > fade_duration) {
                PLATFORM.screen().fade(1.f);
                state_ = State::wait;
                timer_ = 0;

                network::packet::PlayMusic p;
                p.music_id_.set(1);
                network::transmit(p);

            } else {
                const auto amount = smoothstep(0.f, fade_duration, timer_);
                PLATFORM.screen().schedule_fade(amount);
            }
            break;
        }

        case State::wait:
            APP.player_island().set_position(
                {Fixnum::from_integer(10), Fixnum::from_integer(374)});
            APP.opponent_island()->set_position(
                {Fixnum::from_integer(10 + 16 * 14),
                 Fixnum::from_integer(374)});
            return scene_pool::alloc<FadeInScene>();
            break;
        }

        return null_scene();
    }


private:
    enum class State {
        fade_out,
        wait,
    } state_ = State::fade_out;

    Time timer_ = 0;
};



} // namespace skyland
