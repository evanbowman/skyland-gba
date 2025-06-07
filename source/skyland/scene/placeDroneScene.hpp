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

#include "allocator.hpp"
#include "skyland/entity.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "worldScene.hpp"



namespace skyland
{



class PlaceDroneScene : public ActiveWorldScene
{
public:
    PlaceDroneScene(RoomCoord origin,
                    const DroneMeta* drone_class,
                    bool near = true);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


private:
    DynamicMemory<bool[16][16]> matrix_;
    Time cursor_anim_timer_ = 0;
    u8 cursor_anim_frame_ = 0;
    RoomCoord origin_;
    bool near_;

    Optional<Text> message_;
    const DroneMeta* drone_class_;
};



} // namespace skyland
