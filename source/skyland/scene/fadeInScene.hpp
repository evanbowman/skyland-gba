#pragma once


#include "worldScene.hpp"



namespace skyland
{



class FadeInScene : public WorldScene
{
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override;

    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

private:
    Microseconds timer_ = 0;
};



} // namespace skyland
