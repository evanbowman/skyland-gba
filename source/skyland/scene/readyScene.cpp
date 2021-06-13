#include "readyScene.hpp"
#include "constructionScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "worldScene.hpp"


namespace skyland {


ScenePtr<Scene> ReadyScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = WorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    if (pfrm.keyboard().down_transition<Key::alt_2>()) {
        return scene_pool::alloc<ConstructionScene>();
    }

    if (pfrm.keyboard().pressed<Key::up>()) {
        auto view = pfrm.screen().get_view();
        const auto center = view.get_center();
        if (center.y > -40) {
            view.set_center({center.x, center.y - 1});
        }
        pfrm.screen().set_view(view);
    }
    if (pfrm.keyboard().pressed<Key::down>()) {
        auto view = pfrm.screen().get_view();
        const auto center = view.get_center();
        if (center.y < 0) {
            view.set_center({center.x, center.y + 1});
        }
        pfrm.screen().set_view(view);
    }

    return null_scene();
}


} // namespace skyland
