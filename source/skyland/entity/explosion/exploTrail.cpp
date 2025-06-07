////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "exploTrail.hpp"
#include "skyland/entity/explosion/explosion.hpp"



namespace skyland
{



u8 ExploTrail::s_count_ = 0;



ExploTrail::ExploTrail(const Vec2<Fixnum>& pos,
                       int angle,
                       Fixnum speed,
                       Time duration)
    : Entity({}), duration_(duration)
{
    ++s_count_;

    if (s_count_ > 4) {
        kill();
    }

    sprite_.set_position(pos);
    sprite_.set_alpha(Sprite::Alpha::transparent);

    Vec2<Float> dirv = {0, -1};
    if (rng::choice<2>(rng::critical_state)) {
        dirv = rotate(dirv, rng::choice<35>(rng::critical_state));
    } else {
        dirv = rotate(dirv, rng::choice<360 - 35>(rng::critical_state));
    }

    speed_ = {Fixnum(dirv.x) * 0.0001_fixed * speed,
              Fixnum(dirv.y) * 0.0002_fixed * speed};
}



void ExploTrail::update(Time delta)
{
    if (delta == 0) {
        return;
    }

    timer1_ += delta;
    timer2_ += delta;

    if (timer1_ > duration_) {
        kill();
    }

    auto pos = sprite_.get_position();
    pos = pos + speed_ * APP.delta_fp();
    sprite_.set_position(pos);

    if (APP.game_speed() == GameSpeed::fast) {
        speed_.y = speed_.y + 0.00001_fixed;
    } else if (APP.game_speed() == GameSpeed::slow) {
        speed_.y = speed_.y + 0.0000025_fixed;
    } else {
        speed_.y = speed_.y + 0.000005_fixed;
    }

    auto max_y = APP.player_island().origin().y;
    max_y += Fixnum::from_integer(16 * 16 + 32);
    if (pos.y > max_y) {
        kill();
    }

    if (timer2_ > milliseconds(60)) {
        timer2_ -= milliseconds(60);

        auto pos = sprite_.get_position();

        pos = rng::sample<4>(pos, rng::utility_state);

        if (auto ent = APP.alloc_entity<Explosion>(pos)) {
            APP.effects().push(std::move(ent));
        }
    }
}



} // namespace skyland
