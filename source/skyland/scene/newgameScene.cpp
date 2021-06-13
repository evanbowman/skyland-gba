#include "newgameScene.hpp"
#include "readyScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene> NewgameScene::update(Platform&, App& app, Microseconds delta)
{
    app.coins() = 1000;
    app.terrain_cost() = 500;

    return scene_pool::alloc<ReadyScene>();
}



}
