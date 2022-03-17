#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class GlossaryViewerModule : public Module<GlossaryViewerModule>
{
public:

    GlossaryViewerModule(int page = 0) :
        page_(page)
    {
    }


    static SystemString module_name()
    {
        return SystemString::module_glossary;
    }


    static u16 icon()
    {
        return 1304;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void set_next_scene(DeferredScene next)
    {
        next_scene_.emplace(next);
    }


    static bool stop_sound()
    {
        return false;
    }


private:
    void load_page(Platform& pfrm, int page);


    std::optional<Text> item_name_;
    std::optional<Text> item_details_;
    std::optional<TextView> item_description_;
    std::optional<Text> dependency_text_;


    std::optional<DeferredScene> next_scene_;


    int page_ = 0;


    static Factory factory_;
};



} // namespace skyland
