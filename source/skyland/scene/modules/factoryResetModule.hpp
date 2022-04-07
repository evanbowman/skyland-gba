#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class FactoryResetModule : public Module<FactoryResetModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_factory_reset;
    }


    static u16 icon()
    {
        return 968;
    }


    static bool run_scripts()
    {
        return false;
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    std::optional<TextView> text_;
    int key_count_ = 0;

    static Factory factory_;
};



} // namespace skyland
