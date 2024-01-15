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


#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class SmokePuff : public Entity
{
public:
    SmokePuff(const Vec2<Fixnum>& position, u16 tile = 27);


    ~SmokePuff();


    static u8 get_instance_count();


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


    void rewind(Time delta)
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


    void update(Time delta)
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
    Time timer_ = 0;
    Fixnum shrink_amount_ = Fixnum::from_integer(-24);
};



} // namespace skyland
