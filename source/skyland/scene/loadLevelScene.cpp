#include "loadLevelScene.hpp"
#include "fadeInScene.hpp"
#include "fullscreenDialogScene.hpp"
#include "globals.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void set_island_positions(Island& left_island, Island& right_island)
{
    // Now, you may be wondering, why did I put the player and opponent islands
    // at strange y-positions like 374? I forked one of my other GBA projects
    // when developing the jam version of SKYLAND. The engine from BlindJump
    // used larger background maps, so a y of 300 was more centrally located. In
    // any event, the starting y position will always be some weird constant no
    // matter what I do. I suppose I could have started the number at zero, but
    // I didn't know how big the islands were going to be originally, so I gave
    // myself extra space to work with.

    left_island.set_position({10, 374});
    // Pretty much as far away as possible, without wrapping across the screen.
    right_island.set_position(
        {Float(250 + 16 * (10 - right_island.terrain().size())), 374});
}



void LoadLevelScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    app.player_island().repaint(pfrm, app);
}



void LoadLevelScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);
}



static SHARED_VARIABLE(zone1_coin_yield);
static SHARED_VARIABLE(zone2_coin_yield);
static SHARED_VARIABLE(zone3_coin_yield);
static SHARED_VARIABLE(zone4_coin_yield);



void prep_level(Platform& pfrm, App& app)
{
    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;
    auto& far_cursor_loc =
        std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
    far_cursor_loc.x = 0;
    far_cursor_loc.y = 14;


    app.victory_coins() = 0;
    app.pause_count() = 0;
    app.level_timer().reset(0);
    app.level_coins_spent() = 0;
    app.player().rooms_built_ = 0;
    app.player().rooms_lost_ = 0;

    app.persistent_data().score_.set(
        std::max((s32)0, app.persistent_data().score_.get()));


    app.level_begin_score() = app.persistent_data().score_.get();


    if (app.opponent_island()) {
        app.opponent_island()->set_drift(pfrm, app, -0.000025f);

        app.opponent_island()->repaint(pfrm, app);

        set_island_positions(app.player_island(), *app.opponent_island());

        app.player_island().set_float_timer(0);

        app.opponent_island()->set_float_timer(
            std::numeric_limits<Microseconds>::max() / 2);

        for (auto& room : app.opponent_island()->rooms()) {
            if (app.zone() < 2) {
                app.victory_coins() +=
                    (0.01f * zone1_coin_yield) * (*room->metaclass())->cost();
            } else if (app.zone() < 3) {
                app.victory_coins() +=
                    (0.01f * zone2_coin_yield) * (*room->metaclass())->cost();
            } else if (app.zone() < 4) {
                app.victory_coins() +=
                    (0.01f * zone3_coin_yield) * (*room->metaclass())->cost();
            } else {
                app.victory_coins() +=
                    (0.01f * zone4_coin_yield) * (*room->metaclass())->cost();
            }
        }

        pfrm.load_tile1_texture("tilesheet_enemy_0");
        write_custom_graphics(pfrm, app);
        app.opponent_island()->render_exterior(pfrm, app);
    }
}



ScenePtr<Scene>
LoadLevelScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    const auto loc = app.current_world_location();
    auto& node = app.world_graph().nodes_[loc];

    for (auto& room : app.player_island().rooms()) {
        room->detach_drone(pfrm, app, true);
    }
    app.player_island().drones().clear();


    switch (node.type_) {
    case WorldGraph::Node::Type::neutral:
    case WorldGraph::Node::Type::neutral_hidden:
    default: {
        app.invoke_script(pfrm, "/scripts/event/neutral.lisp");
        break;
    }

    case WorldGraph::Node::Type::quest:
        app.invoke_script(pfrm, "/scripts/event/quest.lisp");
        break;

    case WorldGraph::Node::Type::corrupted:
        app.invoke_script(pfrm, "/scripts/event/storm_king.lisp");
        break;


    case WorldGraph::Node::Type::exit:
    case WorldGraph::Node::Type::hostile:
    case WorldGraph::Node::Type::hostile_hidden: {
        app.invoke_script(pfrm, "/scripts/event/hostile.lisp");
        break;
    }
    }

    if (node.type_ == WorldGraph::Node::Type::corrupted) {
        pfrm.speaker().play_music("unaccompanied_wind", 0);

    } else {
        if (not pfrm.speaker().is_music_playing("sb_solecism")) {
            pfrm.speaker().play_music("sb_solecism", 0);
        }
    }

    prep_level(pfrm, app);

    if (app.dialog_buffer()) {
        auto buffer = std::move(*app.dialog_buffer());
        app.dialog_buffer().reset();

        auto future_scene = [&pfrm,
                             storm_king = node.type_ ==
                                          WorldGraph::Node::Type::corrupted] {
            if (storm_king) {
                pfrm.screen().set_shader([](int p, ColorConstant k, int) {
                    if (p == 1) {
                        return grayscale_shader(p, k, 76);
                    } else {
                        auto k1 = contrast_shader(p, k, -10);
                        auto k2 = grayscale_shader(p, k1, 128);
                        return k2;
                    }
                });
            }

            return scene_pool::alloc<FadeInScene>();
        };
        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
                                                        future_scene);
    }

    return scene_pool::alloc<FadeInScene>();
}



} // namespace skyland
