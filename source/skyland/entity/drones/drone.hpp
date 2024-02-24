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


    void update(Time delta) override;


    void rewind(Time delta) override;



    Drone* cast_drone() override
    {
        return this;
    }


    virtual void ___rewind___ability_used()
    {
    }


    virtual void ___rewind___finished_reload()
    {
        ___rewind___ability_used();
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
    void __override_state(State state, Time duration, Time timer)
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



    void set_target(const RoomCoord& target, bool target_near = false);


    void drop_target();


    Island* parent() const
    {
        return parent_;
    }


    Island* destination() const
    {
        return destination_;
    }


    virtual const char* name() const = 0;


    virtual ScenePtr<Scene> select() = 0;


    virtual Time reload_time_remaining() const = 0;


    u8 metaclass_index() const
    {
        return metaclass_index_;
    }


    void apply_damage(Health amount) override;


    Time timer() const
    {
        return timer_;
    }


    Time duration() const
    {
        return duration_;
    }


    virtual void display_on_hover(Platform::Screen& screen,

                                  const RoomCoord& cursor)
    {
    }


    void display(Platform::Screen& screen);


protected:
    u8 state_ = State::launch;

    Time timer_ = 0;
    Time duration_ = 0;

    void update_sprite();

private:
    Island* parent_;
    Island* destination_;
    RoomCoord grid_pos_;
    Vec2<s16> anchor_;

protected:
    Optional<RoomCoord> target_;
    u8 target_near_ : 1;

private:
    u8 metaclass_index_ : 7;
};



} // namespace skyland
