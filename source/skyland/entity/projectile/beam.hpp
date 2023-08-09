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


    void set_timer(Microseconds value)
    {
        timer_ = value;
    }


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&, Vec2<u8>) override;


    void on_collision(Platform& pfrm, App& app, Entity&) override;


private:
    void destroy(Platform& pfrm, App& app, bool explosion) override;

    void record_destroyed(Platform& pfrm, App& app);

    Microseconds timer_ = 0;
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
