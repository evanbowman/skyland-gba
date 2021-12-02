#pragma once


#include "skyland/scene/module.hpp"
#include "memory/buffer.hpp"
#include "graphics/overlay.hpp"



namespace skyland {



class FileBrowserModule : public Module<FileBrowserModule> {
public:
    static const char* module_name()
    {
        return "File Browser";
    }


    static u16 icon()
    {
        return 952;
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    Buffer<Text, 4> lines_;
    std::optional<Text> info_;

    static Factory factory_;
};



}
