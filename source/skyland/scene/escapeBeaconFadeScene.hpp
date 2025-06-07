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

#include "fadeOutScene.hpp"
#include "skyland/island.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland
{



class EscapeBeaconFadeScene : public WorldScene
{
public:
    EscapeBeaconFadeScene(bool player_escaped) : player_escaped_(player_escaped)
    {
    }



    ScenePtr update(Time delta) override
    {
        WorldScene::update(delta);

        constexpr auto fade_duration = milliseconds(2000);

        if (timer_ > 0 and not island_hidden_) {
            if (player_escaped_) {
                player_island().set_hidden(true);
            } else if (opponent_island()) {
                opponent_island()->set_hidden(true);
            }
            island_hidden_ = true;
        }

        timer_ += delta;
        if (timer_ > fade_duration + milliseconds(300)) {
            return make_scene<FadeOutScene>();
        }

        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        PLATFORM.screen().schedule_fade(amount, ColorConstant::electric_blue);

        return null_scene();
    }


private:
    Time timer_ = 0;
    bool player_escaped_ = false;
    bool island_hidden_ = false;
};



} // namespace skyland
