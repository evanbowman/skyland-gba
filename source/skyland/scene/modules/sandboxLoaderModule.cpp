#include "sandboxLoaderModule.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void prep_level(Platform& pfrm, App& app);



ScenePtr<Scene> SandboxLoaderModule::update(Platform& pfrm,
                                            App& app,
                                            Microseconds delta)
{
    app.invoke_script(pfrm, "/scripts/sandbox.lisp");

    pfrm.speaker().play_music("sb_solecism", 0);

    prep_level(pfrm, app);
    app.player_island().render_exterior(pfrm, app);

    app.game_mode() = App::GameMode::sandbox;

    return scene_pool::alloc<FadeInScene>();
}



SandboxLoaderModule::Factory SandboxLoaderModule::factory_;



}
