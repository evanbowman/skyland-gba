#pragma once


#include "skyland/entity/drones/drone.hpp"
#include "worldScene.hpp"



namespace skyland {



class SalvageDroneScene : public ActiveWorldScene
{
public:
    SalvageDroneScene(SharedEntityRef<Drone> drone) : drone_(drone)
    {
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    SharedEntityRef<Drone> drone_;

    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
};



} // namespace skyland
