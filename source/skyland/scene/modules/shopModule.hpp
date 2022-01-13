#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class ShopModule : public Module<ShopModule> {
public:
    static const char* module_name()
    {
        return "Market";
    }


    static u16 icon()
    {
        return 1192;
    }


    static bool run_scripts()
    {
        return false;
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

private:
    void repaint(Platform&, App&);

    int selector_ = 0;

    std::optional<Text> text_;

    static Factory factory_;
};



} // namespace skyland
