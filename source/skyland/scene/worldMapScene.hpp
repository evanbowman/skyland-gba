#pragma once


#include "skyland/scene.hpp"



namespace skyland {



class WorldMapScene : public Scene {
public:

    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev_scene) override;


    void exit(Platform&, App&, Scene& next_scene) override;


private:

};



}
