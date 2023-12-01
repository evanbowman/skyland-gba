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



    ScenePtr<Scene> update(Microseconds delta) override
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
            return scene_pool::alloc<FadeOutScene>();
        }

        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        PLATFORM.screen().schedule_fade(amount, ColorConstant::electric_blue);

        return null_scene();
    }


private:
    Microseconds timer_ = 0;
    bool player_escaped_ = false;
    bool island_hidden_ = false;
};



} // namespace skyland
