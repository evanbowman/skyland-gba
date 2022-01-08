#pragma once


#include "skyland/scene/module.hpp"



namespace skyland {



class SandboxLoaderModule : public Module<SandboxLoaderModule> {
public:

    static const char* module_name()
    {
        return "Battle Sandbox";
    }


    static u16 icon()
    {
        return 1176;
    }


    static bool run_scripts()
    {
        return true;
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    static bool enable_custom_scripts()
    {
        return true;
    }


    static Factory factory_;
};



} // namespace skyland
