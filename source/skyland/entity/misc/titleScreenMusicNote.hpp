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


    void rewind(Platform&, App&, Microseconds delta)
    {
        // Rewind unimplemented!
        kill();
    }


    void update(Platform&, App&, Microseconds delta)
    {
        delta *= 2;
        timer_ += delta;

        auto pos = sprite_.get_position();

        x_anchor_ -= Float(delta) * 0.00002f;
        auto wave_amp =
            (4 * (int)sine(4 * 3.14f * 0.001f * timer_ + 180)) / INT16_MAX;
        pos.x = x_anchor_ + (wave_flip_ ? -wave_amp : wave_amp);
        pos.y -= +Float(delta) * 0.00002f;

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
    Microseconds timer_ = 0;
    bool wave_flip_;
};



} // namespace skyland
