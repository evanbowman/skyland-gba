#pragma once

#include "skyland/scene.hpp"
#include "worldScene.hpp"
#include "memory/buffer.hpp"




namespace skyland {



class ConstructionScene : public WorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


private:
    using Coord = Vec2<u8>;

    Buffer<Coord, 12> construction_sites_;
};




}
