////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    ScenePtr update(Time delta) override
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
            return make_scene<FadeInScene>();
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
