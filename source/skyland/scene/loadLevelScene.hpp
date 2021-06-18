#pragma once



#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland {



class LoadLevelScene : public Scene {
public:
    LoadLevelScene()
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


};



}
