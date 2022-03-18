#pragma once



#include "skyland/entity/drones/drone.hpp"
#include "worldScene.hpp"



namespace skyland
{



class RepairDroneRangeScene : public ActiveWorldScene
{
public:
    RepairDroneRangeScene(SharedEntityRef<Drone> drone) : drone_(drone)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform& pfrm, App& app) override;


public:
    SharedEntityRef<Drone> drone_;
    std::optional<Text> description_;
};



} // namespace skyland
