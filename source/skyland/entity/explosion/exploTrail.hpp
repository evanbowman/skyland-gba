////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
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



class ExploTrail : public Entity
{
public:
    ExploTrail(const Vec2<Fixnum>& pos, int angle, Fixnum speed, Time duration);


    void update(Time delta) override;


    void rewind(Time delta) override
    {
        kill();
    }


    ~ExploTrail()
    {
        --s_count_;
    }


    bool entity_oom_deletable() const override
    {
        return false;
    }


private:
    Time duration_;
    Vec2<Fixnum> speed_;
    Time timer1_ = 0;
    Time timer2_ = 0;

    static u8 s_count_;
};



} // namespace skyland
