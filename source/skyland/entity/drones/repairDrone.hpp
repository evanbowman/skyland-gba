////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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


    void update(App& app, Microseconds delta) override
    {
        if (parent() == app.opponent_island()) {
            sprite_.set_texture_index(71);
        }

        switch (state_) {
        case Drone::State::launch:
            Drone::update(app, delta);
            break;

        case Drone::State::ready:
            update_sprite(app);
            state_ = State::active;
            timer_ = 0;
            break;

        case State::active:
            duration_ += delta;
            update_sprite(app);
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
                    room->heal(app, 6);
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


    ScenePtr<Scene> select(App&) override
    {
        PLATFORM.speaker().play_sound("drone_beep", 1);
        return scene_pool::alloc<RepairDroneRangeScene>(shared_from_this());
    }


    static const auto reload_time = seconds(1);


    Microseconds reload_time_remaining() const override
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
