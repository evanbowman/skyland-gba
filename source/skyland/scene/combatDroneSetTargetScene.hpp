#pragma once

#include "skyland/entity/drones/drone.hpp"
#include "worldScene.hpp"



namespace skyland {



class CombatDroneSetTargetScene : public ActiveWorldScene {
public:
    CombatDroneSetTargetScene(SharedEntityRef<Drone> drone)
        : drone_(drone), near_(true)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


private:
    SharedEntityRef<Drone> drone_;

    Buffer<SharedEntityRef<Drone>, 20> targets_;

    Vec2<u8> cursor_loc_;
    bool near_;

    int selector_ = 0;

    std::optional<Text> text_;
};



} // namespace skyland
