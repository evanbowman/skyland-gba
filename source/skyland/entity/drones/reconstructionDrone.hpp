////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "drone.hpp"
#include "skyland/rooms/reconstructionQueue.hpp"



namespace skyland
{



class ReconstructionDrone : public Drone
{
public:
    ReconstructionDrone(Island* parent,
                        Island* destination,
                        const RoomCoord& grid_pos);


    static const char* get_name()
    {
        return "reconstruction-drone";
    }


    static Coins cost()
    {
        return 0;
    }


    void apply_damage(Health amount) override;


    void update(Time delta) override;


    const char* name() const override
    {
        return get_name();
    }


    static u16 icon()
    {
        return 3336;
    }


    static u16 unsel_icon()
    {
        return 3224;
    }


    static bool spawn_near()
    {
        return true;
    }


    static const auto reload_time = seconds(1);


    void on_rewind_drone_destroyed() override
    {
        return_to_ = position();
    }


    ScenePtr select() override;


    Time reload_time_remaining() const override
    {
        if (state_ == State::launch_2) {
            return reload_time;
        }
        return reload_time - timer_;
    }


    bool ignores_damage() override
    {
        return true;
    }


    enum State : u8 {
        __derived = Drone::State::ready,
        active,
        launch_2,
    };

    s8 recons_index_ = -1;
    u8 dodge_frames_ = 0;
    RoomCoord return_to_;
};



} // namespace skyland
