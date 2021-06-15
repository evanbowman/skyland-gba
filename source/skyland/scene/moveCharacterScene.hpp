#pragma once


#include "bulkAllocator.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class MoveCharacterScene : public WorldScene {
public:
    MoveCharacterScene(Platform& pfrm);


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    DynamicMemory<bool[16][16]> matrix_;
    Microseconds cursor_anim_timer_ = 0;
    u8 cursor_anim_frame_ = 0;
};



} // namespace skyland
