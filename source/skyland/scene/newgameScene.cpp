#include "newgameScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/alloc_entity.hpp"
#include "loadLevelScene.hpp"



namespace skyland {



// void set_island_positions(Island& left_island, Island& right_island)
// {
//     left_island.set_position({10, 374});
//     // Pretty much as far away as possible, without wrapping across the screen.
//     right_island.set_position(
//         {Float(350 + 16 * (10 - right_island.terrain().size())), 374});
// }



ScenePtr<Scene>
NewgameScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    pfrm.screen().fade(1.f);

    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");


    app.player_island().add_room<Core>(pfrm, {1, 13});

    app.coins() = 2500;
    app.terrain_cost() = 500;

    app.player().missile_ammo() = 3;


    auto chr = alloc_entity<BasicCharacter>(&app.player_island(),
                                            Vec2<u8>({2, 14}));
    while (not chr);
    app.player_island().add_character(std::move(chr));


    return scene_pool::alloc<LoadLevelScene>("test.lisp");
}



} // namespace skyland
