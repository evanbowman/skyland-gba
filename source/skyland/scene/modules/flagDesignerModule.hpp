#pragma once


#include "skyland/scene/module.hpp"
#include "skyland/flag.hpp"




namespace skyland {



class FlagDesignerModule : public Module<FlagDesignerModule> {
public:
    static const char* module_name()
    {
        return "Flag Designer";
    }


    static u16 icon()
    {
        return 952;
    }


    void enter(Platform&, App&, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:

    bool ready_ = false;

    u16 palette_[16];


    void show(Platform&, App&);


    Vec2<u8> cursor_;
    int color_ = 0;


    static Factory factory_;
};



}
