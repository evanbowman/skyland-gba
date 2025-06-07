////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "exploSpawner.hpp"

#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/explosion/explosion3.hpp"



namespace skyland
{



void ExploSpawner::update(Time delta)
{
    timer1_ += delta;
    timer2_ += delta;

    if (timer1_ > milliseconds(900)) {
        kill();
    }

    if (timer2_ > milliseconds(150)) {
        timer2_ -= milliseconds(150);
        if (timer1_ < milliseconds(600)) {
            APP.camera()->shake(2);
        }
        auto pos = sprite_.get_position();
        pos = rng::sample<16>(pos, rng::utility_state);


        bool is_offscreen =
            (pos.x.as_integer() <
             PLATFORM.screen().get_view().int_center().x + 8 - (1 * 16) / 2) or
            (pos.x.as_integer() - (1 * 16) / 2 >
             (int)(PLATFORM.screen().get_view().int_center().x +
                   PLATFORM.screen().size().x));

        if (is_offscreen) {
            kill();
            return;
        }

        if (auto ent = APP.alloc_entity<Explosion3>(pos, 90 / 2, 0)) {
            ent->set_speed(
                {0.0_fixed, Fixnum::from_integer(-1) * 0.0001_fixed});

            APP.effects().push(std::move(ent));
        }
    }
}



ExploSpawner* ExploSpawner::create(const Vec2<Fixnum>& pos)
{
    if (auto ent = APP.alloc_entity<ExploSpawner>(pos)) {
        auto ret = ent.get();
        APP.effects().push(std::move(ent));
        return ret;
    }
    return nullptr;
}



} // namespace skyland
