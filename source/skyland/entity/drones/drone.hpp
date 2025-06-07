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

#include "memory/rc.hpp"
#include "skyland/blockChecksum.hpp"
#include "skyland/coord.hpp"
#include "skyland/entity.hpp"
#include "skyland/scene.hpp"
#include "skyland/targetQueue.hpp"



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


    virtual void on_rewind_drone_destroyed()
    {
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



    void
    set_target(const RoomCoord& target, bool target_pinned, bool target_near);
    void
    set_target(const TargetQueue& target, bool target_pinned, bool target_near);


    Optional<RoomCoord> get_target() const;
    bool target_near() const;


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


    virtual ScenePtr select() = 0;


    virtual Time reload_time_remaining() const = 0;


    u8 metaclass_index() const
    {
        return metaclass_index_;
    }


    void apply_damage(Health amount) override;


    virtual bool ignores_damage();


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


    bool target_pinned() const;


    BlockChecksum& target_checksum()
    {
        return target_checksum_;
    }


    void set_shielded(bool is_shielded);


    Room* attached_to();


    void clear_target_queue();


    void update_targets();


    void __rewind_push_target_queue(const RoomCoord& c);


private:
    Island* parent_;
    Island* destination_;

protected:
    Time timer_ = 0;
    Time duration_ = 0;

    void update_sprite();

    Vec2<s16> anchor_;

private:
    BlockChecksum target_checksum_ = 0;
    RoomCoord grid_pos_;

protected:
    TargetQueue target_queue_;
    bool target_near_ : 1 = false;
    bool target_pinned_ : 1 = false;
    bool shielded_ : 1 = false;
    bool suppress_time_stream_ : 1 = false;
    u8 state_ = State::launch;

private:
    u8 metaclass_index_ : 7;
};



} // namespace skyland
