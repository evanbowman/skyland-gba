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

#include "exploSpawner.hpp"

#include "skyland/entity/explosion/explosion3.hpp"



namespace skyland
{



void ExploSpawner::update(Microseconds delta)
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
        if (auto ent = APP.alloc_entity<Explosion3>(pos, 90 / 2, 0)) {

            ent->set_speed(
                {0.0_fixed, Fixnum::from_integer(-1) * 0.0001_fixed});

            APP.effects().push(std::move(ent));
        }
    }
}



void ExploSpawner::create(const Vec2<Fixnum>& pos)
{
    if (auto ent = APP.alloc_entity<ExploSpawner>(pos)) {
        APP.effects().push(std::move(ent));
    }
}



} // namespace skyland
