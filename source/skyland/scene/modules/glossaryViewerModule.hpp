#pragma once

#include "skyland/scene/module.hpp"
#include "graphics/overlay.hpp"



namespace skyland {



class GlossaryViewerModule : public Module<GlossaryViewerModule> {
public:
    static const char* module_name()
    {
        return "Glossary";
    }


    static u16 icon()
    {
        return 1000;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:

    void load_page(Platform& pfrm, int page);


    std::optional<Text> item_name_;
    std::optional<Text> item_details_;
    std::optional<TextView> item_description_;


    int page_ = 0;


    static Factory factory_;
};



}
