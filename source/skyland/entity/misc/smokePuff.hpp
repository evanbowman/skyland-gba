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


#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class SmokePuff : public Entity
{
public:
    SmokePuff(const Vec2<Fixnum>& position, u16 tile = 27) : Entity({{}, {}})
    {
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(tile);
        sprite_.set_position(position);
        sprite_.set_alpha(Sprite::Alpha::translucent);
        sprite_.set_origin({8, 8});
    }


    // Used during rewind. It's simply wasteful to store data for effects like
    // this in memory. So jump the animation to its endpoint, ad rewind.
    void jump_to_end()
    {
        timer_ = seconds(3);

        auto pos = sprite_.get_position();

        pos.x -= Fixnum(+Float(seconds(3)) * 0.00001f);
        pos.y -= Fixnum(+Float(seconds(3)) * 0.00001f);

        const s16 shrink_amount =
            interpolate(-450, -24, Float(timer_) / seconds(3));

        sprite_.set_scale({shrink_amount, shrink_amount});

        sprite_.set_position(pos);
    }


    void rewind(App&, Microseconds delta)
    {
        // In some older code I was accidentally updating effects twice per
        // frame. Multiply delta x2 until I get around to fixing all the timers.
        delta *= 2;

        timer_ -= delta;

        if (timer_ < 0) {
            kill();
            return;
        }

        auto pos = sprite_.get_position();

        pos.x += Fixnum::from_integer(delta) * 0.00001_fixed;
        pos.y += Fixnum::from_integer(delta) * 0.00001_fixed;

        while (delta >= 16666 / 4) {
            shrink_amount_ += 1.774929_fixed;
            delta -= 16666;
        }

        const s16 shrink_amount =
            interpolate(-450, -24, Float(timer_) / seconds(3));

        shrink_amount_ = shrink_amount;

        sprite_.set_scale({shrink_amount, shrink_amount});

        sprite_.set_position(pos);
    }


    void update(App& app, Microseconds delta)
    {
        delta *= 2;
        timer_ += delta;

        auto pos = sprite_.get_position();

        pos.x -= Fixnum::from_integer(delta) * 0.00001_fixed;
        pos.y -= Fixnum::from_integer(delta) * 0.00001_fixed;


        while (delta >= 16666 / 4) {
            shrink_amount_ -= 0.614929_fixed;
            delta -= 16666 / 4;
        }

        sprite_.set_scale({(s16)shrink_amount_.as_integer(),
                           (s16)shrink_amount_.as_integer()});

        sprite_.set_position(pos);

        if (timer_ > seconds(3)) {
            this->kill();
        }
    }


private:
    Microseconds timer_ = 0;
    Fixnum shrink_amount_ = Fixnum::from_integer(-24);
};



} // namespace skyland
