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


#include "bird.hpp"



namespace skyland
{



class SmallBird : public Bird
{
public:
    SmallBird(const Vec2<Float>& position, Float speed)
        : Bird({{}, {}}), timer1_(0), timer2_(0), speed_(speed)
    {
        sprite_.set_position(position);
        sprite_.set_texture_index(28);
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_origin({8, 8});
        sprite_.set_priority(3);
    }


    void update(Platform&, App&, Microseconds delta) override
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
        pos.x += speed_ * delta;
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
    Microseconds timer1_;
    Microseconds timer2_;
    Float speed_;
};



} // namespace skyland
