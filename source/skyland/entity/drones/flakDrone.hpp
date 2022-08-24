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
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class FlakDrone : public Drone
{
public:
    FlakDrone(Island* parent, Island* destination, const RoomCoord& grid_pos)
        : Drone(get_name(), parent, destination, grid_pos)
    {
        sprite_.set_texture_index(66);
    }


    static const char* get_name()
    {
        return "flak-drone";
    }


    const char* name() const override
    {
        return get_name();
    }


    static u16 icon()
    {
        return 1112;
    }


    static u16 unsel_icon()
    {
        return 1128;
    }


    ScenePtr<Scene> select(Platform& pfrm, App& app) override
    {
        pfrm.speaker().play_sound("drone_beep", 1);
        return scene_pool::alloc<WeaponSetTargetScene>(
            position(), destination() == &app.player_island());
    }


    static const auto reload_time = milliseconds(8000);


    Microseconds reload_time_remaining() const override
    {
        if (state_ == Drone::State::launch) {
            return reload_time;
        }
        return reload_time - timer_;
    }


    static Coins cost()
    {
        return 0;
    }


    void ___rewind___ability_used(Platform& pfrm, App& app) override
    {
        if (state_ not_eq Drone::State::launch) {
            timer_ = reload_time;
        }
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (parent() == app.opponent_island()) {
            sprite_.set_texture_index(69);
        }

        switch (state_) {
        case Drone::State::launch:
            Drone::update(pfrm, app, delta);
            break;

        case Drone::State::ready:
            update_sprite(pfrm, app);
            state_ = State::wait;
            timer_ = 0;
            break;

        case State::wait:
            duration_ += delta;
            update_sprite(pfrm, app);
            if (timer_ > reload_time) {
                if (target_) {
                    if (not app.opponent_island()) {
                        return;
                    }

                    Island* target_island;
                    if (parent() == &app.player_island()) {
                        target_island = app.opponent_island();
                    } else {
                        target_island = &app.player_island();
                    }

                    if (target_) {

                        auto start = sprite_.get_position();
                        start.x += 8;
                        start.y += 8;
                        auto target = target_island->origin();
                        target.x += target_->x * 16 + 8;
                        target.y += target_->y * 16 + 8;

                        auto c = app.alloc_entity<Flak>(
                            pfrm, start, target, parent(), position());
                        if (c) {
                            app.camera()->shake(4);
                            parent()->projectiles().push(std::move(c));
                        }
                        timer_ = 0;
                        state_ = Drone::State::ready;
                    }
                }
            } else {
                timer_ += delta;

                if (timer_ > reload_time) {
                    time_stream::event::DroneReloadComplete e;
                    e.x_pos_ = position().x;
                    e.y_pos_ = position().y;
                    e.destination_near_ = destination() == &app.player_island();
                    app.time_stream().push(app.level_timer(), e);
                }
            }

            break;
        }
    }

    enum State : u8 {
        __derived = Drone::State::ready,
        wait,
    };
};



} // namespace skyland
