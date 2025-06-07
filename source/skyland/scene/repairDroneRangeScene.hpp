////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


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


    ScenePtr update(Time delta) override;


    void display() override;


public:
    SharedEntityRef<Drone> drone_;
    Optional<Text> description_;
};



} // namespace skyland
