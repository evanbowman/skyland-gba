#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class CreditsModule : public Module<CreditsModule> {
public:
    static const char* module_name()
    {
        return "Credits";
    }


    static u16 icon()
    {
        return 1304;
    }


    static bool run_scripts()
    {
        return false;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:

    void load_page(Platform& pfrm, int page);


    int page_ = 0;


    static Factory factory_;
};



}
