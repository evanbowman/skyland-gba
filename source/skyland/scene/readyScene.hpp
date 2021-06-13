#pragma once



#include "worldScene.hpp"



namespace skyland {



class ReadyScene : public WorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
};



}
