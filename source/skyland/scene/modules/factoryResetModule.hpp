#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class FactoryResetModule : public Module<FactoryResetModule> {
public:
    static const char* module_name()
    {
        return "Factory Reset";
    }


    static u16 icon()
    {
        return 968;
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    std::optional<TextView> text_;
    int key_count_ = 0;

    static Factory factory_;
};



} // namespace skyland
