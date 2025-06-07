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



class Curveshot : public Projectile
{
public:
    Curveshot(const Vec2<Float>& position,
              const Vec2<Float>& target,
              Island* source,
              Island* dest,
              const RoomCoord& origin_tile,
              const RoomCoord& dest_tile);


    void set_step_vector(const Vec2<Float>& val)
    {
        step_vector_ = val;
    }


    void set_timer(Time value)
    {
        timer_ = value;
    }


    void update(Time delta) override;


    void rewind(Time delta) override;


    void on_collision(Room&) override;


    void on_collision(Entity&) override;


private:
    void destroy(bool explosion);

    Float y_base_ = 0;
    Time timer_ = 0;
    Time time_to_target_ = 0;
    Vec2<Float> step_vector_;
    Island* source_;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    RoomCoord origin_tile_;
    u8 height_;
};



} // namespace skyland
