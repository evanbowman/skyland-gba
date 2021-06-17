#include "skyland/scene_pool.hpp"
#include "newgameScene.hpp"
#include "worldMapScene.hpp"
#include "platform/platform.hpp"
#include "graphics/overlay.hpp"



namespace skyland {



ScenePtr<Scene> WorldMapScene::update(Platform& pfrm, App&, Microseconds delta)
{
    if (pfrm.keyboard().down_transition<Key::action_1>()) {
        return scene_pool::alloc<NewgameScene>();
    }

    return null_scene();
}



void WorldMapScene::enter(Platform& pfrm, App&, Scene& prev_scene)
{
    pfrm.load_overlay_texture("overlay_world_map");

    auto st = calc_screen_tiles(pfrm);

    for (int x = 0; x < st.x; ++x) {
        for (int y = 0; y < st.y; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 88);
        }
    }
}



void WorldMapScene::exit(Platform& pfrm, App&, Scene& next_scene)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    pfrm.load_overlay_texture("overlay");

    pfrm.fill_overlay(0);
}



}
