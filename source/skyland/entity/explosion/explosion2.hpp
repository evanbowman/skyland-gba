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
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class Explosion2 : public Entity
{
public:
    static const int start_index = 19 * 2;


    Explosion2(const Vec2<Fixnum>& position,
               u8 half_angle,
               u8 speed_int,
               int priority = 1)
        : Entity({{}, {}}), half_angle_(half_angle), speed_int_(speed_int)
    {
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w16_h16);
        sprite_.set_texture_index(start_index);
        sprite_.set_origin({8, 8});
        sprite_.set_priority(priority);
    }


    void update(Time delta) override
    {
        timer_ += delta * 2;

        auto pos = sprite_.get_position();
        pos = pos + speed_ * APP.delta_fp();
        sprite_.set_position(pos);

        if (timer_ > milliseconds(200)) {
            timer_ = 0;

            auto index = sprite_.get_texture_index();
            if (index < start_index + 5) {
                sprite_.set_texture_index(index + 1);
            } else {
                time_stream::event::Explosion e;
                e.x_.set(pos.x.as_integer());
                e.y_.set(pos.y.as_integer());
                e.half_angle_ = half_angle_;
                e.spd_ = speed_int_;
                APP.push_time_stream(e);
                kill();
            }
        }

        int min_x = PLATFORM.screen().get_view().int_center().x - 48;
        int max_x = PLATFORM.screen().get_view().int_center().x +
                    PLATFORM.screen().size().x + 48;
        int max_y = 700;
        int min_y = 450;

        if (pos.y.as_integer() > max_y or pos.y.as_integer() < min_y or
            pos.x.as_integer() > max_x or pos.x.as_integer() < min_x) {
            // kill();
            sprite_.set_alpha(Sprite::Alpha::transparent);
        }
    }


    void rewind(Time delta) override
    {
        timer_ -= delta * 2;

        auto pos = sprite_.get_position();
        pos = pos - speed_ * APP.delta_fp();
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

        int min_x = PLATFORM.screen().get_view().int_center().x - 48;
        int max_x = PLATFORM.screen().get_view().int_center().x +
                    PLATFORM.screen().size().x + 48;
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


    void restore(time_stream::event::Explosion e)
    {
        timer_ = milliseconds(200);
        sprite_.set_texture_index(start_index + 5);

        const auto angle = e.half_angle_ * 2;
        auto dir = rotate({1, 0}, angle);
        dir = dir * (((e.spd_ + 1 / 2.f) * 1.5f) * 0.00005f);
        speed_.x = Fixnum(dir.x);
        speed_.y = Fixnum(dir.y);
        half_angle_ = e.half_angle_;
        speed_int_ = e.spd_;
    }


    void set_speed(const Vec2<Fixnum>& speed)
    {
        speed_ = speed;
    }


private:
    Time timer_ = 0;
    Vec2<Fixnum> speed_;
    u8 half_angle_;
    u8 speed_int_;
};



} // namespace skyland
