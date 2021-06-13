#include "worldScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene> WorldScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.updateParallax(delta);

    pfrm.set_scroll(app.player_island().layer(),
                    -app.player_island().get_position().cast<u16>().x,
                    -app.player_island().get_position().cast<u16>().y -
                    app.player_island().get_ambient_movement());


    if (pfrm.keyboard().down_transition<Key::alt_1>()) {
        if (app.player_island().interior_visible()) {
            pfrm.load_tile0_texture("tilesheet");
            app.player_island().render_exterior(pfrm);
        } else {
            pfrm.load_tile0_texture("tilesheet_interior");
            app.player_island().render_interior(pfrm);
        }
    }

    app.player_island().update(pfrm, app, delta);

    return null_scene();
}



}
