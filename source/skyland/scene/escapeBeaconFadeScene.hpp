#pragma once

#include "fadeOutScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/island.hpp"



namespace skyland
{



class EscapeBeaconFadeScene : public WorldScene
{
public:

    EscapeBeaconFadeScene(bool player_escaped) :
        player_escaped_(player_escaped)
    {

    }



    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta) override
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



}
