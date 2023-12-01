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


    void rewind(Microseconds delta)
    {
        // Rewind unimplemented!
        kill();
    }


    void update(Microseconds delta)
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
    Microseconds timer_ = 0;
    bool wave_flip_;
};



} // namespace skyland
