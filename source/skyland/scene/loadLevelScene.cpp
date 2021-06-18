#include "loadLevelScene.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "fadeInScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "globals.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/configure_island.hpp"



namespace skyland {



void set_island_positions(Island& left_island, Island& right_island)
{
    left_island.set_position({10, 374});
    // Pretty much as far away as possible, without wrapping across the screen.
    right_island.set_position(
        {Float(350 + 16 * (10 - right_island.terrain().size())), 374});
}



ScenePtr<Scene> LoadLevelScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    const auto loc = app.current_map_location();
    auto& node = app.world_map().matrix_[loc.x][loc.y];


    switch (node.type_) {
    case WorldMap::Node::Type::storm_clear:
    case WorldMap::Node::Type::clear:
        lisp::dostring(pfrm.load_file_contents("scripts", "test.lisp"));
        break;

    case WorldMap::Node::Type::storm_hostile:
        lisp::dostring(pfrm.load_file_contents("scripts", "test2.lisp"));
        app.hostile_nodes_visited() += 1;
        break;

    case WorldMap::Node::Type::hostile:
        lisp::dostring(pfrm.load_file_contents("scripts", "test.lisp"));
        app.hostile_nodes_visited() += 1;
        break;


    case WorldMap::Node::Type::null:
        pfrm.fatal("world map mem corrupt");
    }



    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    if (app.opponent_island()) {
        app.opponent_island()->set_drift(-0.000025f);

        set_island_positions(app.player_island(), *app.opponent_island());

        app.player_island().set_float_timer(0);

        app.opponent_island()->set_float_timer(std::numeric_limits<Microseconds>::max() / 2);

        for (auto& room : app.opponent_island()->rooms()) {
            app.victory_coins() += 0.25f * (*room->metaclass())->cost();
        }

        app.opponent_island()->show_flag(true);
        app.opponent_island()->repaint(pfrm);
    }

    return scene_pool::alloc<FadeInScene>();
}



}
