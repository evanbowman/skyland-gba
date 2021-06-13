#pragma once

#include "skyland/scene.hpp"
#include "worldScene.hpp"
#include "memory/buffer.hpp"




namespace skyland {



class ConstructionScene : public WorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


private:


    void find_construction_sites(Platform&, App&);


    using Coord = Vec2<u8>;

    Buffer<Coord, 12> construction_sites_;
    u32 selector_ = 0;
};




}
