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

#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/room_metatable.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ConstructDroneScene : public ActiveWorldScene
{
public:
    ConstructDroneScene(RoomCoord position) : position_(position)
    {
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void draw();


private:
    RoomCoord position_;
    Optional<Text> text_;
    int selector_ = 0;
};



} // namespace skyland
