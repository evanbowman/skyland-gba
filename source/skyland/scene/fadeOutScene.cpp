#include "fadeOutScene.hpp"
#include "selectTutorialScene.hpp"
#include "skyland/autopilotPlayer.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland {



ScenePtr<Scene>
FadeOutScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    timer_ += delta;

    constexpr auto fade_duration = milliseconds(800);
    if (timer_ > fade_duration) {
        pfrm.screen().fade(1.f);
        switch (app.game_mode()) {
        case App::GameMode::tutorial:
            return scene_pool::alloc<SelectTutorialScene>();

        case App::GameMode::adventure:
            return scene_pool::alloc<ZoneImageScene>();

        case App::GameMode::challenge:
            return scene_pool::alloc<TitleScreenScene>(2);

        case App::GameMode::sandbox:
            return scene_pool::alloc<TitleScreenScene>(3);

        case App::GameMode::multiplayer:
            return scene_pool::alloc<TitleScreenScene>();
        }
    } else {
        const auto amount = smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().fade(amount);
    }

    return null_scene();
}



} // namespace skyland
