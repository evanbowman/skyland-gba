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


#include "number/fixnum.hpp"
#include "projectile.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class Beam : public Projectile
{
public:
    Beam(const Vec2<Fixnum>& position,
         const Vec2<Fixnum>& target,
         Island* source,
         const RoomCoord& origin_tile,
         int index);


    void restore_blocks_hit(const time_stream::event::BeamDestroyed& e);


    void set_step_vector(const Vec2<Fixnum>& val)
    {
        step_vector_ = val;
    }


    void set_timer(Time value)
    {
        timer_ = value;
    }


    void update(Time delta) override;


    void rewind(Time delta) override;


    void on_collision(Room&, Vec2<u8>) override;


    void on_collision(Entity&) override;


    bool entity_oom_deletable() const override
    {
        // Some beam segments are just visual effects and don't deal damage, but
        // we don't want the beam effect to be erased for lower priority
        // explosion effects, for example.
        return false;
    }


private:
    void destroy(bool explosion) override;

    void record_destroyed();

    Time timer_ = 0;
    Vec2<Fixnum> step_vector_;
    Island* source_;

    Buffer<RoomCoord, 8> damaged_;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    RoomCoord origin_tile_;

    bool hit_opponent_ = false;
    u8 index_;
};



} // namespace skyland
