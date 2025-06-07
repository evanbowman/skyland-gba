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



class CombatDroneSetTargetScene : public ActiveWorldScene
{
public:
    CombatDroneSetTargetScene(SharedEntityRef<Drone> drone)
        : drone_(drone), near_(true)
    {
    }


    ScenePtr update(Time delta) override;


    void display() override;


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


private:
    SharedEntityRef<Drone> drone_;

    Buffer<SharedEntityRef<Drone>, 20> targets_;

    RoomCoord cursor_loc_;
    bool near_;

    int selector_ = 0;

    Optional<Text> text_;
};



} // namespace skyland
