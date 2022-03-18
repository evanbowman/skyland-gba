#pragma once


#include "skyland/flag.hpp"
#include "skyland/paint.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class FlagDesignerModule : public Module<FlagDesignerModule>, public Paint
{

public:
    static SystemString module_name()
    {
        return SystemString::module_flag_designer;
    }


    static u16 icon()
    {
        return 952;
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    static bool run_scripts()
    {
        return false;
    }


    u8 get_pixel(App& app, u8 x, u8 y) override;
    void set_pixel(App& app, u8 x, u8 y, u8 value) override;


    void show(Platform&, App&) override;


private:
    static Factory factory_;
};



} // namespace skyland
