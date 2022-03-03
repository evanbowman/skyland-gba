#pragma once


#include "skyland/scene.hpp"



namespace skyland {



class NewgameScene : public Scene
{
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
};



} // namespace skyland
