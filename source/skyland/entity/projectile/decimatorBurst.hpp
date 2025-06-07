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
#include "skyland/skyland.hpp"



namespace skyland
{



class DecimatorBurst : public Projectile
{
public:
    DecimatorBurst(const Vec2<Fixnum>& position,
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


    void on_collision(Room&, Vec2<u8> origin) override;


    void on_collision(Entity&) override;


    int burst_index_ = 0;


    DecimatorBurst* cast_decimator_burst()
    {
        return this;
    }


    RoomCoord origin_tile() const
    {
        return origin_tile_;
    }


private:
    Time timer_ = 0;
    Vec2<Fixnum> step_vector_;
    Island* source_;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    RoomCoord origin_tile_;
};



} // namespace skyland
