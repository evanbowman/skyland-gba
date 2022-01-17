#include "fadeInScene.hpp"
#include "readyScene.hpp"
#include "scriptHookScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene>
FadeInScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    timer_ += delta;

    constexpr auto fade_duration = milliseconds(800);
    if (timer_ > fade_duration) {
        if (app.game_mode() not_eq App::GameMode::tutorial and
            not pfrm.network_peer().is_connected()) {

            const auto loc = app.current_world_location();
            auto& node = app.world_graph().nodes_[loc];

            if (node.type_ == WorldGraph::Node::Type::hostile or
                node.type_ == WorldGraph::Node::Type::corrupted or
                app.game_mode() == App::GameMode::challenge) {
                app.game_speed() = GameSpeed::stopped;
            }
            // FIXME!!!!!!!
        }
        pfrm.screen().fade(0.f);
        auto future_scene = scene_pool::make_deferred_scene<ReadyScene>();
        return scene_pool::alloc<ScriptHookScene>("after-fadein-hook",
                                                  future_scene);
    } else {
        const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().fade(amount);
    }

    return null_scene();
}



} // namespace skyland
