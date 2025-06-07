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


#include "bird.hpp"



namespace skyland
{



class SmallBird : public Bird
{
public:
    SmallBird(const Vec2<Fixnum>& position, Float speed)
        : Bird({{}, {}}), timer1_(0), timer2_(0), speed_(speed)
    {
        sprite_.set_position(position);
        sprite_.set_texture_index(28);
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_origin({8, 8});
        sprite_.set_priority(3);
    }


    void update(Time delta) override
    {
        timer1_ += delta;
        timer2_ += delta;

        if (timer1_ > milliseconds(90)) {
            timer1_ = 0;

            auto kf = sprite_.get_texture_index();
            if (kf > 28 + 6) {
                kf = 28;
            } else {
                ++kf;
            }
            sprite_.set_texture_index(kf);
        }

        auto pos = sprite_.get_position();
        pos.x += Fixnum(speed_ * delta);
        sprite_.set_position(pos);

        if (timer2_ > seconds(10)) {
            kill();
        }
    }


    auto age() const
    {
        return timer2_;
    }


    Float speed() const
    {
        return speed_;
    }


private:
    Time timer1_;
    Time timer2_;
    Float speed_;
};



} // namespace skyland
