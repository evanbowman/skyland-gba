////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    static const auto reload_time = seconds(1);


    void on_rewind_drone_destroyed() override
    {
        return_to_ = position();
    }


    ScenePtr select() override;


    Time reload_time_remaining() const override
    {
        if (state_ == Drone::State::launch) {
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
    };

    s8 recons_index_ = -1;
    u8 dodge_frames_ = 0;
    RoomCoord return_to_;
};



} // namespace skyland
