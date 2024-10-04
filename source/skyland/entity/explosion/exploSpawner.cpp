////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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

#include "exploSpawner.hpp"

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
