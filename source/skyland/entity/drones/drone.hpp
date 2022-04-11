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

#include "memory/rc.hpp"
#include "skyland/coord.hpp"
#include "skyland/entity.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class Island;



class Drone : public Entity, public IntrusiveRcControlBlock<Drone>
{
public:
    Drone(const char* name,
          Island* parent,
          Island* destination,
          const RoomCoord& grid_pos);


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;



    virtual void ___rewind___ability_used(Platform& pfrm, App& app)
    {
    }


    virtual void ___rewind___finished_reload(Platform& pfrm, App& app)
    {
        ___rewind___ability_used(pfrm, app);
    }


    enum State : u8 {
        launch,
        ready,
    };


    u8 state() const
    {
        return state_;
    }


    // Intended for the rewind logic, nothing more. Generally, do not call.
    void
    __override_state(State state, Microseconds duration, Microseconds timer)
    {
        state_ = state;
        duration_ = duration;
        timer_ = timer;
    }


    void __set_health(Health health)
    {
        health_ = health;
    }


    const RoomCoord& position() const
    {
        return grid_pos_;
    }


    void set_movement_target(const RoomCoord& position);



    void set_target(Platform& pfrm,
                    App& app,
                    const RoomCoord& target,
                    bool target_near = false);


    void drop_target(Platform& pfrm, App& app);


    Island* parent() const
    {
        return parent_;
    }


    Island* destination() const
    {
        return destination_;
    }


    virtual const char* name() const = 0;


    virtual ScenePtr<Scene> select(Platform& pfrm, App&) = 0;


    virtual Microseconds reload_time_remaining() const = 0;


    u8 metaclass_index() const
    {
        return metaclass_index_;
    }


    void apply_damage(Platform& pfrm, App& app, Health amount) override;


    Microseconds timer() const
    {
        return timer_;
    }


    Microseconds duration() const
    {
        return duration_;
    }


protected:
    u8 state_ = State::launch;

    Microseconds timer_ = 0;
    Microseconds duration_ = 0;

    void update_sprite(Platform& pfrm, App& app);

private:
    Island* parent_;
    Island* destination_;
    RoomCoord grid_pos_;
    Vec2<s16> anchor_;

protected:
    std::optional<RoomCoord> target_;
    u8 target_near_ : 1;

private:
    u8 metaclass_index_ : 7;
};



} // namespace skyland
