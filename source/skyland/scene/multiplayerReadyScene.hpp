#pragma once


#include "fadeInScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland
{



class MultiplayerReadyScene : public WorldScene
{
public:
    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        WorldScene::update(pfrm, app, delta);

        timer_ += delta;

        switch (state_) {
        case State::fade_out: {
            constexpr auto fade_duration = milliseconds(800);
            if (timer_ > fade_duration) {
                pfrm.screen().fade(1.f);
                state_ = State::wait;
                timer_ = 0;
            } else {
                const auto amount = smoothstep(0.f, fade_duration, timer_);
                pfrm.screen().fade(amount);
            }
            break;
        }

        case State::wait:
            app.player_island().set_position({10, 374});
            app.opponent_island()->set_position({10 + 16 * 14, 374});
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

    Microseconds timer_ = 0;
};



} // namespace skyland
