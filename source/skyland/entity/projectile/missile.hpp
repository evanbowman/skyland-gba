////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
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
    Missile(const Vec2<Float>& position,
            const Vec2<Float>& target,
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


protected:
    virtual void destroy(Platform& pfrm, App& app);

private:
    Microseconds timer_ = 0;
    Float target_x_;

    Island* source_;
    u8 source_x_;
    u8 source_y_;

    State state_ = State::rising;
};



extern SharedVariable missile_damage;



class RocketBomb : public Missile
{
public:
    RocketBomb(const Vec2<Float>& position,
               const Vec2<Float>& target,
               u8 source_x,
               u8 source_y,
               Island* source)
        : Missile(position, target, source_x, source_y, source)
    {
        sprite_.set_texture_index(88);
    }



    void on_collision(Platform& pfrm, App& app, Room&) override;


protected:
    void destroy(Platform& pfrm, App& app) override;
};



} // namespace skyland
