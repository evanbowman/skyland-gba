#pragma once


#include "scene/newgameScene.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class InitScene : public Scene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override
    {


        return null_scene();
    }
};



} // namespace skyland
