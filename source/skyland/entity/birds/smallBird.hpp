////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


    void update(Microseconds delta) override
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
    Microseconds timer1_;
    Microseconds timer2_;
    Float speed_;
};



} // namespace skyland
