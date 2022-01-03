#pragma once



#include "worldScene.hpp"
#include "skyland/entity/drones/drone.hpp"



namespace skyland {



class RepairDroneRangeScene : public ActiveWorldScene {
public:
    RepairDroneRangeScene(SharedEntityRef<Drone> drone) : drone_(drone)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform& pfrm, App& app) override;


public:
    WeakEntityRef<Drone> drone_;
    std::optional<Text> description_;
};



} // namespace skyland
