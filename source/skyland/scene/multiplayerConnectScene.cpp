#include "multiplayerConnectScene.hpp"
#include "skyland/opponent/multiplayerPeer.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene_pool.hpp"
#include "script/lisp.hpp"
#include "fadeInScene.hpp"
#include "globals.hpp"



namespace skyland {


void set_island_positions(Island& left_island, Island& right_island);



void MultiplayerConnectScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().fade(1.f);
    text_.emplace(pfrm, "session connecting...", OverlayCoord{1, 1});

    lisp::dostring(pfrm.load_file_contents("scripts", "multiplayer_init.lisp"),
                   [&pfrm](lisp::Value& v) {
                       pfrm.fatal(lisp::Error::get_string(v.error_.code_));
                   });


    if (app.opponent_island()) {
        set_island_positions(app.player_island(),
                             *app.opponent_island());

        app.opponent_island()->repaint(pfrm);
    } else {
        pfrm.fatal("multi missing opp island");
    }

    auto& terrain_0 = app.player_island().terrain();
    auto& terrain_1 = app.opponent_island()->terrain();

    terrain_0.clear();
    terrain_1.clear();

    terrain_0.push_back(13);
    terrain_1.push_back(13);

    for (int i = 0; i < 5; ++i) {
        terrain_0.push_back(12);
        terrain_1.push_back(12);
    }

    terrain_0.push_back(14);
    terrain_1.push_back(14);


    app.player_island().repaint(pfrm);

    app.coins() = 10000;

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    pfrm.delta_clock().reset();
    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");

    std::get<SkylandGlobalData>(globals()).multiplayer_prep_timer_ = 0;
    std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_ = 120;
}



void MultiplayerConnectScene::exit(Platform&, App&, Scene& next)
{
    text_.reset();
}



ScenePtr<Scene> MultiplayerConnectScene::update(Platform& pfrm,
                                                App& app,
                                                Microseconds delta)
{
    if (not ready_) {
        ready_ = true;
        return null_scene();
    }
    pfrm.network_peer().listen();

    if (not pfrm.network_peer().is_connected()) {
        pfrm.fatal("failed to connect to multiplayer peer");
    } else {
        app.swap_opponent<MultiplayerPeer>();
        rng::critical_state = 42;
        return scene_pool::alloc<FadeInScene>();
    }

    return null_scene();
}



}
