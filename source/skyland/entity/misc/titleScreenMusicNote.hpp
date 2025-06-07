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


#include "number/random.hpp"
#include "skyland/entity.hpp"



namespace skyland
{



class TitleScreenMusicNote : public Entity
{
public:
    TitleScreenMusicNote(const Vec2<Fixnum>& position, u16 tile)
        : Entity({{}, {}})
    {
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(tile);
        sprite_.set_position(position);
        sprite_.set_origin({8, 8});

        x_anchor_ = position.x;

        wave_flip_ = rng::choice<2>(rng::utility_state);
    }


    void rewind(Time delta)
    {
        // Rewind unimplemented!
        kill();
    }


    void update(Time delta)
    {
        delta *= 2;
        timer_ += delta;

        auto pos = sprite_.get_position();

        x_anchor_ -= Fixnum(Float(delta) * 0.00002f);
        auto wave_amp =
            (4 * (int)sine(4 * 3.14f * 0.001f * timer_ + 180)) / INT16_MAX;
        pos.x = x_anchor_ + Fixnum((wave_flip_ ? -wave_amp : wave_amp));
        pos.y -= Fixnum(+Float(delta) * 0.00002f);

        sprite_.set_priority(3);


        const s16 shrink_amount =
            interpolate(-450, 0, Float(timer_) / seconds(10));

        sprite_.set_scale({shrink_amount, shrink_amount});

        sprite_.set_position(pos);

        if (timer_ > milliseconds(2500)) {
            this->kill();
        }
    }


private:
    Fixnum x_anchor_;
    Time timer_ = 0;
    bool wave_flip_;
};



} // namespace skyland
