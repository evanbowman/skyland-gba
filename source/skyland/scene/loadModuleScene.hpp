#pragma once

#include "skyland/scene.hpp"
#include "memory/buffer.hpp"
#include "graphics/overlay.hpp"



namespace skyland {



class LoadModuleScene : public Scene {
public:

    void enter(Platform&, App&, Scene& prev) override;


    void show_modules(Platform&, int page);


    Buffer<Text, 8> temp_;

};



}
