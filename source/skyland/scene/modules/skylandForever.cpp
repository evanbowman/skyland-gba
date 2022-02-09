#include "skylandForever.hpp"
#include "skyland/opponent/procgenEnemyAI.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void prep_level(Platform& pfrm, App& app);



void SkylandForever::enter(Platform& pfrm, App& app, Scene& prev)
{
    app.game_mode() = App::GameMode::skyland_forever;
}



void SkylandForever::exit(Platform& pfrm, App& app, Scene& prev)
{
    app.set_coins(pfrm, 0);
    app.player_island().init_terrain(pfrm, 4);

    app.persistent_data().score_.set(0);

    pfrm.speaker().play_music("sb_solecism", 0);

    app.invoke_script(pfrm, "/scripts/skyland_forever.lisp");

    prep_level(pfrm, app);
    app.player_island().set_position({10, 374});

    app.reset_opponent_island(pfrm);
    app.swap_opponent<ProcgenEnemyAI>();


    show_island_exterior(pfrm, app, &app.player_island());
    show_island_exterior(pfrm, app, app.opponent_island());

    pfrm.load_overlay_texture("overlay");
    pfrm.system_call("v-parallax", (void*)true);

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
}



ScenePtr<Scene>
SkylandForever::update(Platform& pfrm, App& app, Microseconds delta)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
    return scene_pool::alloc<FadeInScene>();
}



SkylandForever::Factory SkylandForever::factory_;



} // namespace skyland
