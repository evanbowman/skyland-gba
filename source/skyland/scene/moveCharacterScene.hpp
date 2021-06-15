#pragma once


#include "skyland/scene.hpp"
#include "worldScene.hpp"
#include "bulkAllocator.hpp"



namespace skyland {



class MoveCharacterScene : public WorldScene {
public:
    MoveCharacterScene(Platform& pfrm);


    void enter(Platform&, App&, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    DynamicMemory<bool[16][16]> matrix_;
};



} // namespace skyland
