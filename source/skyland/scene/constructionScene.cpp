#include "constructionScene.hpp"
#include "worldScene.hpp"
#include "skyland/scene_pool.hpp"
#include "platform/platform.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene> ConstructionScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (pfrm.keyboard().down_transition<Key::alt_2>()) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



void ConstructionScene::enter(Platform& pfrm, App& app, Scene&)
{
    bool matrix[16][16];

    app.player_island().plot_construction_zones(matrix);

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y]) {
                construction_sites_.push_back({x, y});
            }
        }
    }

    for (auto& site : construction_sites_) {
        pfrm.set_tile(app.player_island().layer(), site.x, site.y, 1);
    }
}



void ConstructionScene::exit(Platform& pfrm, App& app, Scene&)
{
    app.player_island().repaint(pfrm);
}



}
