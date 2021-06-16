#pragma once


#include "worldScene.hpp"




namespace skyland {



class FadeInScene : public WorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

private:
    Microseconds timer_ = 0;
};



}
