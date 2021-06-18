#pragma once



#include "worldScene.hpp"
#include "string.hpp"



namespace skyland {



class LoadLevelScene : public WorldScene {
public:
    LoadLevelScene()
    {
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;



    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

};



}
