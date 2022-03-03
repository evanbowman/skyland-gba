#pragma once

#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class LoadModuleScene : public Scene
{
public:
    void enter(Platform&, App&, Scene& prev) override;


    void show_modules(Platform&, int page);


    Buffer<Text, 8> temp_;
};



} // namespace skyland
