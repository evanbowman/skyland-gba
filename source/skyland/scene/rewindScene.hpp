#pragma once

#include "skyland/scene.hpp"



namespace skyland {



class RewindScene : public Scene {
public:


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;



    void display(Platform&, App&) override;


};



}
