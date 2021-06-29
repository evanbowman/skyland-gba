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
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland {



ScenePtr<Scene>
NewgameScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    app.player_island().rooms().clear();

    lisp::dostring(pfrm.load_file_contents("scripts", "newgame.lisp"),
                   [&pfrm](lisp::Value& v) {
                       pfrm.fatal(lisp::Error::get_string(v.error_.code_));
                   });


    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");

    app.current_map_location() = {0, 1};
    app.world_map().generate();

    app.coins() = 2500;
    app.terrain_cost() = 500;

    app.zone() = 1;

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    app.player_island().set_position({10, 374});


    return scene_pool::alloc<ZoneImageScene>();
}



} // namespace skyland
