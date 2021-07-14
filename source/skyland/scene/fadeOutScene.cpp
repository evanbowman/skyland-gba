#include "fadeOutScene.hpp"
#include "skyland/scene_pool.hpp"
#include "zoneImageScene.hpp"
#include "skyland/autopilotPlayer.hpp"
#include "skyland/skyland.hpp"
#include "selectTutorialScene.hpp"



namespace skyland {



ScenePtr<Scene>
FadeOutScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    timer_ += delta;

    constexpr auto fade_duration = milliseconds(800);
    if (timer_ > fade_duration) {
        pfrm.screen().fade(1.f);
        if (app.tutorial_mode()) {
            return scene_pool::alloc<SelectTutorialScene>();
        } else {
            return scene_pool::alloc<ZoneImageScene>();
        }
    } else {
        const auto amount = smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().fade(amount);
    }

    return null_scene();
}



} // namespace skyland
