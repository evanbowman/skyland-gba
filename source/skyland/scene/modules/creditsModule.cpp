#include "creditsModule.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland {




void CreditsModule::enter(Platform& pfrm, App& app, Scene& prev)
{

}



void CreditsModule::exit(Platform& pfrm, App& app, Scene& next)
{

}



ScenePtr<Scene> CreditsModule::update(Platform& pfrm,
                                      App& app,
                                      Microseconds delta)
{
    if (player(app).key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<TitleScreenScene>();
    }



    return null_scene();
}



CreditsModule::Factory CreditsModule::factory_;



}
