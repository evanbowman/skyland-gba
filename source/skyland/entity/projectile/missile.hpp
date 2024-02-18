////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
