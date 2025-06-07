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


#include "skyland/entity.hpp"



namespace skyland
{



class ExpFlash : public Entity
{
public:
    static const int start_index = 52;


    ExpFlash(const Vec2<Fixnum>& position);


    void update(Time delta) override;


    void rewind(Time delta) override
    {
        kill(); // TODO...
    }


private:
    Time timer_ = 0;
    int anim_index_ = 0;
};



class Explosion : public Entity
{
public:
    static const int start_index = 19 * 2;


    Explosion(const Vec2<Fixnum>& position, int priority = 1);


    void update(Time delta) override;


    void rewind(Time delta) override
    {
        timer_ -= delta * 2;

        if (timer_ < 0) {
            timer_ = milliseconds(55);

            auto index = sprite_.get_texture_index();
            if (index > start_index) {
                sprite_.set_texture_index(index - 1);
            } else {
                kill();
            }
        }
    }


    void seek_end()
    {
        timer_ = milliseconds(55);
        sprite_.set_texture_index(start_index + 5);
    }


private:
    Time timer_ = 0;
};



class TeenyExplosion : public Entity
{
public:
    static const int start_index = 50;


    TeenyExplosion(const Vec2<Fixnum>& position);


    void update(Time delta) override;


    void rewind(Time delta) override
    {
        kill();
    }


private:
    Time timer_ = 0;
    int anim_index_ = 0;
};



void medium_explosion(const Vec2<Fixnum>& position);
void medium_explosion_inv(const Vec2<Fixnum>& position);


struct BigExplosionConfig
{
    int draw_priority_ = 1;
    bool centerflash_ = false;
};


void big_explosion(const Vec2<Fixnum>& position,
                   const BigExplosionConfig& conf = {});

void big_explosion_inv(const Vec2<Fixnum>& position);



} // namespace skyland
