#include "loadModuleScene.hpp"
#include "module.hpp"



namespace skyland
{



void LoadModuleScene::enter(Platform& pfrm, App&, Scene& prev)
{
    show_modules(pfrm, 0);
    pfrm.screen().fade(1.f);
}



void LoadModuleScene::show_modules(Platform& pfrm, int page)
{
    auto factory = detail::_Module::factories_;

    int i = 1;

    while (factory) {
        temp_.emplace_back(pfrm, factory->name(), OverlayCoord{1, (u8)i++});
        factory = factory->next_;
    }
}



} // namespace skyland
