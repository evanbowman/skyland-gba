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


    void set_timer(Time value)
    {
        timer_ = value;
    }


    void update(Time delta) override;


    void rewind(Time delta) override;


    void on_collision(Room&, Vec2<u8>) override;


    void on_collision(Entity& entity) override;


    Fixnum fall_speed();


protected:
    virtual void on_destroy();

    Time timer_ = 0;
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



    void on_collision(Room&, Vec2<u8>) override;



    void burst(const Vec2<Fixnum>& position, Room& origin_room);



protected:
    void on_destroy() override;
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



    void on_collision(Room&, Vec2<u8>) override;
    void on_collision(Entity&) override;



    void burst(const Vec2<Fixnum>& position, Room& origin_room);



protected:
    void spawn_bomblets(Island* source, Vec2<Fixnum> origin);


    void on_destroy() override;
};



class AtomicMissile : public Missile
{
public:
    AtomicMissile(const Vec2<Fixnum>& position,
                  const Vec2<Fixnum>& target,
                  u8 source_x,
                  u8 source_y,
                  Island* source)
        : Missile(position, target, source_x, source_y, source)
    {
        sprite_.set_texture_index(94);
    }



    void on_collision(Room&, Vec2<u8>) override;



    void burst(const Vec2<Fixnum>& position, Room& origin_room);



protected:
    void on_destroy() override;
};



} // namespace skyland
