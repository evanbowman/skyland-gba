#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class SalvageRoomScene : public WorldScene {
public:
    SalvageRoomScene()
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

};



}
