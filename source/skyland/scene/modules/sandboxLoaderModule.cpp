#include "sandboxLoaderModule.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void prep_level(Platform& pfrm, App& app);



ScenePtr<Scene>
SandboxLoaderModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.invoke_script(pfrm, "/scripts/sandbox.lisp");

    // pfrm.speaker().play_music("unaccompanied_wind", 0);
    pfrm.speaker().play_music("sb_solecism", 0);

    prep_level(pfrm, app);
    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");
    app.player_island().render_exterior(pfrm, app);

    app.game_mode() = App::GameMode::sandbox;

    return scene_pool::alloc<FadeInScene>();
}



SandboxLoaderModule::Factory SandboxLoaderModule::factory_;



} // namespace skyland
