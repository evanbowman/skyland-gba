#include "dlcInjectorModule.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland {



ScenePtr<Scene>
DlcInjectorModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    Module::update(pfrm, app, delta);

    pfrm.system_call("dlc-download", nullptr);

    return scene_pool::alloc<TitleScreenScene>();
}



// NOTE: uncomment when we actually implement this module.
// DlcInjectorModule::Factory DlcInjectorModule::factory_;


} // namespace skyland
