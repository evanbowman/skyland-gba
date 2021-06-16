#include "fadeInScene.hpp"
#include "readyScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland {



ScenePtr<Scene> FadeInScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    timer_ += delta;

    constexpr auto fade_duration = milliseconds(800);
    if (timer_ > fade_duration) {
        return scene_pool::alloc<ReadyScene>();
        pfrm.screen().fade(0.f);
    } else {
        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().fade(amount);
    }

    return null_scene();
}




}
