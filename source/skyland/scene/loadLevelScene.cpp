#include "loadLevelScene.hpp"
#include "fadeInScene.hpp"
#include "fullscreenDialogScene.hpp"
#include "globals.hpp"
#include "localization.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void set_island_positions(Island& left_island, Island& right_island)
{
    left_island.set_position({10, 374});
    // Pretty much as far away as possible, without wrapping across the screen.
    right_island.set_position(
        {Float(250 + 16 * (10 - right_island.terrain().size())), 374});
}



void LoadLevelScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);
}


void LoadLevelScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);
}


ScenePtr<Scene>
LoadLevelScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    const auto loc = app.current_map_location();
    auto& node = app.world_map().matrix_[loc.x][loc.y];

    auto on_lisp_error = [&pfrm](lisp::Value& v) {
        pfrm.fatal(lisp::Error::get_string(v.error_.code_));
    };

    switch (node.type_) {
    case WorldMap::Node::Type::storm_clear:
    case WorldMap::Node::Type::clear: {
        lisp::dostring(pfrm.load_file_contents("scripts", "neutral.lisp"),
                       on_lisp_error);
        break;
    }

    case WorldMap::Node::Type::storm_hostile:
    case WorldMap::Node::Type::hostile: {
        lisp::dostring(pfrm.load_file_contents("scripts", "hostile.lisp"),
                       on_lisp_error);
        break;
    }


    case WorldMap::Node::Type::null:
        pfrm.fatal("world map mem corrupt");
    }



    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;


    app.victory_coins() = 0;


    if (app.opponent_island()) {
        app.opponent_island()->set_drift(-0.000025f);

        app.opponent_island()->repaint(pfrm);

        set_island_positions(app.player_island(), *app.opponent_island());

        app.player_island().set_float_timer(0);

        app.opponent_island()->set_float_timer(
            std::numeric_limits<Microseconds>::max() / 2);

        for (auto& room : app.opponent_island()->rooms()) {
            app.victory_coins() += 0.25f * (*room->metaclass())->cost();
        }

        app.opponent_island()->repaint(pfrm);
    }

    pfrm.delta_clock().reset(); // skip large dt from loading lisp scripts...

    if (app.dialog_buffer()) {
        auto buffer = std::move(*app.dialog_buffer());
        app.dialog_buffer().reset();

        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer));
    }

    return scene_pool::alloc<FadeInScene>();
}



} // namespace skyland
