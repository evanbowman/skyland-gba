#pragma once


#include "worldScene.hpp"



namespace skyland {



class SetGamespeedScene : public WorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

    void enter(Platform&, App&, Scene&) override;
    void exit(Platform&, App&, Scene&) override;


private:
    void repaint_selector(Platform& pfrm);

    int selection_ = 0;
};



} // namespace skyland
