#pragma once



#include "string.hpp"
#include "worldScene.hpp"



namespace skyland {



class LoadLevelScene : public ActiveWorldScene
{
public:
    LoadLevelScene()
    {
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;



    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
};



} // namespace skyland
