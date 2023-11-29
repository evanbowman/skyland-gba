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


    void set_timer(Microseconds value)
    {
        timer_ = value;
    }


    void update(Microseconds delta) override;


    void rewind(Microseconds delta) override;


    void on_collision(Room&, Vec2<u8>) override;


    static void
    burst(const Vec2<Fixnum>& position, Room& origin_room, Island* source);


private:
    Microseconds timer_ = 0;
    Vec2<Fixnum> step_vector_;
    Island* source_;

    void record_destroyed();
    void destroy(bool explosion) override;

    void explode();

    Microseconds flicker_time_ = 0;

    bool destroyed_ = false;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    RoomCoord origin_tile_;
};



extern SharedVariable flak_r1_damage;
extern SharedVariable flak_r2_damage;
extern SharedVariable flak_r3_damage;



} // namespace skyland
