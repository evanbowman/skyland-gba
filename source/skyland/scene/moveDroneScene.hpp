#pragma once

#include "bulkAllocator.hpp"
#include "worldScene.hpp"
#include "skyland/entity.hpp"
#include "skyland/entity/drones/drone.hpp"



namespace skyland {



class MoveDroneScene : public ActiveWorldScene {
public:

    MoveDroneScene(Platform& pfrm, Vec2<u8> origin);


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    DynamicMemory<bool[16][16]> matrix_;
    Microseconds cursor_anim_timer_ = 0;
    u8 cursor_anim_frame_ = 0;
    Vec2<u8> origin_;
};



}
