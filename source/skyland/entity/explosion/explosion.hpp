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


#include "skyland/entity.hpp"



namespace skyland
{



class Explosion : public Entity
{
public:
    static const int start_index = 19 * 2;


    Explosion(const Vec2<Fixnum>& position, int priority = 1) : Entity({{}, {}})
    {
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w16_h16);
        sprite_.set_texture_index(start_index);
        sprite_.set_origin({8, 8});
        sprite_.set_priority(priority);
    }


    void update(Microseconds delta) override
    {
        timer_ += delta * 2;

        if (timer_ > milliseconds(55)) {
            timer_ = 0;

            auto index = sprite_.get_texture_index();
            if (index < start_index + 5) {
                sprite_.set_texture_index(index + 1);
            } else {
                kill();
            }
        }
    }


    void rewind(Microseconds delta) override
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
    Microseconds timer_ = 0;
};



void medium_explosion(const Vec2<Fixnum>& position);
void medium_explosion_inv(const Vec2<Fixnum>& position);


void big_explosion(const Vec2<Fixnum>& position, int draw_priority = 1);

void big_explosion_inv(const Vec2<Fixnum>& position);



} // namespace skyland
