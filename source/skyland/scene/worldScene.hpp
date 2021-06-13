#pragma once


#include "skyland/scene.hpp"




namespace skyland {



class WorldScene : public Scene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
};



}
