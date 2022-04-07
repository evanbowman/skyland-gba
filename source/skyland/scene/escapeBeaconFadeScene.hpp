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



    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        WorldScene::update(pfrm, app, delta);

        constexpr auto fade_duration = milliseconds(2000);

        if (timer_ > 0 and not island_hidden_) {
            if (player_escaped_) {
                player_island(app).set_hidden(pfrm, app, true);
            } else if (opponent_island(app)) {
                opponent_island(app)->set_hidden(pfrm, app, true);
            }
            island_hidden_ = true;
        }

        timer_ += delta;
        if (timer_ > fade_duration + milliseconds(300)) {
            return scene_pool::alloc<FadeOutScene>();
        }

        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().schedule_fade(amount, ColorConstant::electric_blue);

        return null_scene();
    }


private:
    Microseconds timer_ = 0;
    bool player_escaped_ = false;
    bool island_hidden_ = false;
};



} // namespace skyland
