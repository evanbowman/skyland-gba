#include "readyScene.hpp"
#include "worldScene.hpp"
#include "platform/platform.hpp"
#include "constructionScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland {



ScenePtr<Scene> ReadyScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = WorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    if (pfrm.keyboard().down_transition<Key::alt_2>()) {
        return scene_pool::alloc<ConstructionScene>();
    }

    return null_scene();
}



}
