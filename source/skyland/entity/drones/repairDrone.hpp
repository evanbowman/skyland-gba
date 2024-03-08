////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
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
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/repairDroneRangeScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class RepairDrone : public Drone
{
public:
    RepairDrone(Island* parent, Island* destination, const RoomCoord& grid_pos)
        : Drone(get_name(), parent, destination, grid_pos)
    {
        sprite_.set_texture_index(70);
        health_ = 50;
    }


    static const char* get_name()
    {
        return "repair-drone";
    }


    static Coins cost()
    {
        return 400;
    }


    void update(Time delta) override
    {
        if (parent() == APP.opponent_island()) {
            sprite_.set_texture_index(71);
        }

        switch (state_) {
        case Drone::State::launch:
            Drone::update(delta);
            break;

        case Drone::State::ready:
            update_sprite();
            state_ = State::active;
            timer_ = 0;
            break;

        case State::active:
            duration_ += delta;
            update_sprite();
            if (timer_ > reload_time) {

                Buffer<Room*, 16> heal_queue;


                auto pos = position();
                for (int x = pos.x - 2; x < pos.x + 3; ++x) {
                    for (int y = pos.y - 2; y < pos.y + 3; ++y) {
                        if (x < 0 or y < 0) {
                            continue;
                        }
                        if (auto room =
                                destination()->get_room({u8(x), u8(y)})) {
                            const bool found = [&] {
                                for (auto pushed : heal_queue) {
                                    if (pushed == room) {
                                        return true;
                                    }
                                }
                                return false;
                            }();

                            if (not found) {
                                heal_queue.push_back(room);
                            }
                        }
                    }
                }

                for (auto& room : heal_queue) {
                    room->heal(6);
                    room->ready();
                }

                timer_ = 0;

            } else {
                timer_ += delta;
            }

            break;
        }
    }


    const char* name() const override
    {
        return get_name();
    }


    static u16 icon()
    {
        return 1144;
    }


    static u16 unsel_icon()
    {
        return 1160;
    }


    ScenePtr select() override
    {
        PLATFORM.speaker().play_sound("drone_beep", 1);
        return make_scene<RepairDroneRangeScene>(shared_from_this());
    }


    static const auto reload_time = seconds(1);


    Time reload_time_remaining() const override
    {
        if (state_ == Drone::State::launch) {
            return reload_time;
        }
        return reload_time - timer_;
    }


    enum State : u8 {
        __derived = Drone::State::ready,
        active,
    };
};



} // namespace skyland
