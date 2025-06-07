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



class SalvageDroneScene : public ActiveWorldScene
{
public:
    SalvageDroneScene(SharedEntityRef<Drone> drone) : drone_(drone)
    {
    }


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


private:
    SharedEntityRef<Drone> drone_;

    Optional<Text> text_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;
};



} // namespace skyland
