#include "newgameScene.hpp"
#include "readyScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "globals.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/cannon.hpp"



namespace skyland {



ScenePtr<Scene> NewgameScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    app.player_island().set_position({10, 374});
    app.player_island().add_room<Core>(pfrm, {1, 13});

    app.coins() = 3500;
    app.terrain_cost() = 500;


    app.encountered_island().emplace(pfrm, Layer::map_1_ext, 3);
    app.encountered_island()->set_position({150, 374});
    app.encountered_island()->set_float_timer(std::numeric_limits<Microseconds>::max() / 2);

    app.encountered_island()->add_room<Core>(pfrm, {1, 13});
    app.encountered_island()->add_room<Cannon>(pfrm, {0, 14});
    app.encountered_island()->add_room<Cannon>(pfrm, {0, 13});


    return scene_pool::alloc<ReadyScene>();
}



} // namespace skyland
