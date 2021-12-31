#pragma once

#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/room_metatable.hpp"
#include "worldScene.hpp"



namespace skyland {



class ConstructDroneScene : public ActiveWorldScene {
public:
    ConstructDroneScene(Vec2<u8> position) : position_(position)
    {
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void draw(Platform&, App&);


private:
    Vec2<u8> position_;
    std::optional<Text> text_;
    int selector_ = 0;
};



} // namespace skyland
