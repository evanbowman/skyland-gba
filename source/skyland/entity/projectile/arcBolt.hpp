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



namespace skyland
{



class Island;



class ArcBolt : public Projectile
{
public:
    ArcBolt(const Vec2<Fixnum>& position,
            const Vec2<Fixnum>& target,
            Island* source,
            const Vec2<u8>& origin_tile);


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


    void set_step_vector(const Vec2<Fixnum>& val)
    {
        step_vector_ = val;
    }


    void set_timer(Microseconds value)
    {
        timer_ = value;
    }


private:
    void destroy(Platform& pfrm, App& app, bool explosion) override;

    Microseconds timer_ = 0;
    Microseconds anim_timer_ = 0;
    Vec2<Fixnum> step_vector_;
    Island* source_;

    Vec2<u8> origin_tile_;
};



} // namespace skyland
