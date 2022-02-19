#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class AchievementViewerModule : public Module<AchievementViewerModule> {
public:
    static SystemString module_name()
    {
        return SystemString::module_achievements;
    }


    static u16 icon()
    {
        return 1576;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    static bool stop_sound()
    {
        return false;
    }


private:
    void load_page(Platform& pfrm, App& app, int page);


    std::optional<Text> item_name_;
    std::optional<Text> item_details_;
    std::optional<TextView> achievement_description_;
    std::optional<Text> achievement_name_;
    std::optional<Text> unlocks_text_;


    int page_ = 0;


    static Factory factory_;
};



} // namespace skyland
