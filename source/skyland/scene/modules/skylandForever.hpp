#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class SkylandForever : public Module<SkylandForever> {
public:
    static const char* module_name()
    {
        return "SKYLAND Forever";
    }


    static u16 icon()
    {
        return 1736;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    static bool enable_custom_scripts()
    {
        return true;
    }


private:
    static Factory factory_;
};



} // namespace skyland
