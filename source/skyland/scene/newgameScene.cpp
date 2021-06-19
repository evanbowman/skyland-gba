#include "newgameScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "script/lisp.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/workshop.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland {



ScenePtr<Scene>
NewgameScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    pfrm.screen().fade(1.f);

    if (not pfrm.keyboard().pressed<Key::action_1>() and
        not pfrm.keyboard().pressed<Key::action_2>()) {
        rng::get(rng::critical_state);
        return null_scene();
    }

    lisp::dostring(pfrm.load_file_contents("scripts", "newgame.lisp"),
                   [&pfrm](lisp::Value& v) {
                       pfrm.fatal(lisp::Error::get_string(v.error_.code_));
                   });


    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");

    app.player_island().rooms().clear();
    app.player_island().add_room<Core>(pfrm, {1, 13});

    app.current_map_location() = {0, 1};
    app.world_map().generate();

    app.coins() = 2500;
    app.terrain_cost() = 500;

    app.zone() = 1;

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    app.player_island().set_position({10, 374});

    auto chr =
        alloc_entity<BasicCharacter>(&app.player_island(), Vec2<u8>({2, 14}));
    while (not chr)
        ;
    app.player_island().add_character(std::move(chr));


    return scene_pool::alloc<WorldMapScene>();
}



} // namespace skyland
