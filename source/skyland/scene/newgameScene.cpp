#include "newgameScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void set_island_positions(Island& left_island, Island& right_island)
{
    left_island.set_position({10, 374});
    // Pretty much as far away as possible, without wrapping across the screen.
    right_island.set_position(
        {Float(350 + 16 * (10 - right_island.terrain().size())), 374});
}



ScenePtr<Scene>
NewgameScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    app.player_island().add_room<Core>(pfrm, {1, 13});

    app.coins() = 3500;
    app.terrain_cost() = 500;


    app.encountered_island().emplace(pfrm, Layer::map_1_ext, 4, app.opponent());
    app.encountered_island()->show_flag(true);
    app.encountered_island()->set_float_timer(
        std::numeric_limits<Microseconds>::max() / 2);

    app.encountered_island()->add_room<Core>(pfrm, {1, 13});
    app.encountered_island()->add_room<Cannon>(pfrm, {0, 14});
    app.encountered_island()->add_room<Cannon>(pfrm, {0, 13});
    app.encountered_island()->add_room<Stairwell>(pfrm, {3, 11});

    app.encountered_island()->set_drift(-0.000025f);

    set_island_positions(app.player_island(), *app.encountered_island());


    return scene_pool::alloc<ReadyScene>();
}



} // namespace skyland
