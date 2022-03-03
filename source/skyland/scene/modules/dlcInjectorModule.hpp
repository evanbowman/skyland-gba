#pragma once


#include "skyland/scene/module.hpp"



namespace skyland {



class DlcInjectorModule : public Module<DlcInjectorModule>
{
public:
    DlcInjectorModule(bool begin_load = false) : begin_load_(begin_load)
    {
    }


    static SystemString module_name()
    {
        return SystemString::module_update_loader;
    }


    static u16 icon()
    {
        return 1192;
    }


    static bool run_scripts()
    {
        return false;
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    bool begin_load_ = false;


    static Factory factory_;
};



} // namespace skyland
