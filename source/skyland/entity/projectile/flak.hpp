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


#include "projectile.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class Flak : public Projectile
{
public:
    Flak(const Vec2<Fixnum>& position,
         const Vec2<Fixnum>& target,
         Island* source,
         const RoomCoord& origin_tile);


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


    static void
    burst(const Vec2<Fixnum>& position, Room& origin_room, Island* source);


private:
    Time timer_ = 0;
    Vec2<Fixnum> step_vector_;
    Island* source_;

    void record_destroyed();
    void destroy(bool explosion) override;

    void explode();

    Time flicker_time_ = 0;

    bool destroyed_ = false;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    RoomCoord origin_tile_;
};



void make_flak_smoke(const Vec2<Fixnum>& pos);



extern SharedVariable flak_r1_damage;
extern SharedVariable flak_r2_damage;
extern SharedVariable flak_r3_damage;



} // namespace skyland
