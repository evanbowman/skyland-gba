#include "newgameScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "script/lisp.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/workshop.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland {



ScenePtr<Scene>
NewgameScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);


    vram_write_flag(pfrm, app.gp_.flag_img_);

    if (save::load(pfrm, app.persistent_data())) {
        save::erase(pfrm);
    } else {

        app.invoke_script(pfrm, "/scripts/newgame.lisp");

        app.current_map_location() = {0, 1};
        app.world_map().generate();

        app.coins() = 2500;
        app.terrain_cost() = 500;

        app.zone() = 1;

        app.persistent_data().total_pauses_.set(0);
        app.persistent_data().total_seconds_.set(0);
        app.persistent_data().score_.set(0);
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    app.player_island().set_position({10, 374});

    // Because interpreting all those lisp scripts ran up a high frame latency.
    pfrm.delta_clock().reset();


    return scene_pool::alloc<ZoneImageScene>();
}



} // namespace skyland
