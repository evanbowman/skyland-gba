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



class ExploSpawner : public Entity
{
public:
    ExploSpawner(const Vec2<Fixnum>& pos) : Entity({})
    {
        sprite_.set_position(pos);
        sprite_.set_alpha(Sprite::Alpha::transparent);
    }


    void update(Time delta) override;


    void rewind(Time delta) override
    {
        kill();
    }


    static ExploSpawner* create(const Vec2<Fixnum>& pos);


    void set_offset(Time delta)
    {
        timer2_ += delta;
    }


private:
    Time timer1_ = 0;
    Time timer2_ = 0;
};



} // namespace skyland
