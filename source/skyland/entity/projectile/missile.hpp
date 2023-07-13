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



namespace skyland
{



class Island;



class Missile : public Projectile
{
public:
    Missile(const Vec2<Fixnum>& position,
            const Vec2<Fixnum>& target,
            u8 source_x,
            u8 source_y,
            Island* source);


    enum class State : u8 {
        rising,
        wait,
        falling,
    };


    void set_state(State state)
    {
        state_ = state;
    }


    void set_timer(Microseconds value)
    {
        timer_ = value;
    }


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


    void on_collision(Platform& pfrm, App& app, Entity& entity) override;


    Fixnum fall_speed(Platform& pfrm);


protected:
    virtual void destroy(Platform& pfrm, App& app);

    Microseconds timer_ = 0;
    Fixnum target_x_;

    Island* source_;
    u8 source_x_;
    u8 source_y_;

    State state_ = State::rising;
};



extern SharedVariable missile_damage;



class RocketBomb : public Missile
{
public:
    RocketBomb(const Vec2<Fixnum>& position,
               const Vec2<Fixnum>& target,
               u8 source_x,
               u8 source_y,
               Island* source)
        : Missile(position, target, source_x, source_y, source)
    {
        sprite_.set_texture_index(88);
    }



    void on_collision(Platform& pfrm, App& app, Room&) override;



    void burst(Platform& pfrm,
               App& app,
               const Vec2<Fixnum>& position,
               Room& origin_room);



protected:
    void destroy(Platform& pfrm, App& app) override;
};



class ClumpMissile : public Missile
{
public:
    ClumpMissile(const Vec2<Fixnum>& position,
                 const Vec2<Fixnum>& target,
                 u8 source_x,
                 u8 source_y,
                 Island* source)
        : Missile(position, target, source_x, source_y, source)
    {
        sprite_.set_texture_index(90);
    }



    void on_collision(Platform& pfrm, App& app, Room&) override;



    void burst(Platform& pfrm,
               App& app,
               const Vec2<Fixnum>& position,
               Room& origin_room);



protected:
    void destroy(Platform& pfrm, App& app) override;
};



} // namespace skyland
