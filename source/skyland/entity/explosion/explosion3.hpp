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
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class Explosion3 : public Entity
{
public:
    static const int start_index = 19;


    Explosion3(const Vec2<Fixnum>& position,
               u8 half_angle,
               u8 speed_int,
               int priority = 1) :
        Entity({{}, {}}),
        half_angle_(half_angle),
        speed_int_(speed_int)
    {
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(start_index);
        sprite_.set_origin({8, 8});
        sprite_.set_priority(priority);
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ += delta * 2;

        auto pos = sprite_.get_position();
        pos = pos + speed_ * app.delta_fp();
        sprite_.set_position(pos);

        if (timer_ > milliseconds(200)) {
            timer_ = 0;

            auto index = sprite_.get_texture_index();
            if (index < start_index + 5) {
                sprite_.set_texture_index(index + 1);
            } else {
                kill();
            }
        }

        int min_x = pfrm.screen().get_view().get_center().x - 48;
        int max_x = pfrm.screen().get_view().get_center().x + pfrm.screen().size().x + 48;
        int max_y = 700;
        int min_y = 450;

        if (pos.y.as_integer() > max_y or pos.y.as_integer() < min_y or
            pos.x.as_integer() > max_x or pos.x.as_integer() < min_x) {
            // kill();
            sprite_.set_alpha(Sprite::Alpha::transparent);
        }
    }


    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ -= delta * 2;

        auto pos = sprite_.get_position();
        pos = pos - speed_ * app.delta_fp();
        sprite_.set_position(pos);

        if (timer_ < 0) {
            timer_ = milliseconds(200);

            auto index = sprite_.get_texture_index();
            if (index > start_index) {
                sprite_.set_texture_index(index - 1);
            } else {
                kill();
            }
        }

        int min_x = pfrm.screen().get_view().get_center().x - 48;
        int max_x = pfrm.screen().get_view().get_center().x + pfrm.screen().size().x + 48;
        int max_y = 700;
        int min_y = 450;

        if (pos.y.as_integer() > max_y or pos.y.as_integer() < min_y or
            pos.x.as_integer() > max_x or pos.x.as_integer() < min_x) {
            // kill();
            sprite_.set_alpha(Sprite::Alpha::transparent);
        } else {
            sprite_.set_alpha(Sprite::Alpha::opaque);
        }
    }


    void set_speed(const Vec2<Fixnum>& speed)
    {
        speed_ = speed;
    }


private:
    Microseconds timer_ = 0;
    Vec2<Fixnum> speed_;
    u8 half_angle_;
    u8 speed_int_;
};



} // namespace skyland
