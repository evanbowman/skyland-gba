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


#include "explosion.hpp"
#include "number/random.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void medium_explosion(const Vec2<Fixnum>& position)
{
    APP.effects().push(APP.alloc_entity<Explosion>(
        rng::sample<18>(position, rng::utility_state)));

    APP.on_timeout(milliseconds(60), [pos = position]() {
        APP.rumble().activate(milliseconds(200));
        APP.effects().push(APP.alloc_entity<Explosion>(
            rng::sample<18>(pos, rng::utility_state)));

        APP.on_timeout(milliseconds(120), [pos = pos]() {
            APP.effects().push(APP.alloc_entity<Explosion>(
                rng::sample<18>(pos, rng::utility_state)));
        });
    });
}



void medium_explosion_inv(const Vec2<Fixnum>& position)
{
    auto first = APP.alloc_entity<Explosion>(
        rng::sample<18>(position, rng::utility_state));
    first->seek_end();
    APP.effects().push(std::move(first));

    APP.on_timeout(milliseconds(60), [pos = position]() {
        APP.rumble().activate(milliseconds(200));
        auto exp = APP.alloc_entity<Explosion>(
            rng::sample<18>(pos, rng::utility_state));
        exp->seek_end();
        APP.effects().push(std::move(exp));

        APP.on_timeout(milliseconds(120), [pos = pos]() {
            auto exp = APP.alloc_entity<Explosion>(
                rng::sample<18>(pos, rng::utility_state));
            exp->seek_end();
            APP.effects().push(std::move(exp));
        });
    });
}



void big_explosion(const Vec2<Fixnum>& position, int draw_priority)
{
    for (int i = 0; i < 4; ++i) {
        APP.effects().push(APP.alloc_entity<Explosion>(
            rng::sample<18>(position, rng::utility_state), draw_priority));
    }

    int p = draw_priority;
    auto ipos = ivec(position);

    APP.on_timeout(milliseconds(90), [pos = ipos, p]() {
        APP.rumble().activate(milliseconds(390));

        for (int i = 0; i < 3; ++i) {
            APP.effects().push(APP.alloc_entity<Explosion>(
                rng::sample<32>(pos.cast<Fixnum>(), rng::utility_state), p));
        }
        APP.on_timeout(milliseconds(90), [pos, p]() {
            for (int i = 0; i < 2; ++i) {
                APP.effects().push(APP.alloc_entity<Explosion>(
                    rng::sample<48>(pos.cast<Fixnum>(), rng::utility_state),
                    p));
            }
            APP.on_timeout(milliseconds(90), [pos, p]() {
                for (int i = 0; i < 1; ++i) {
                    APP.effects().push(APP.alloc_entity<Explosion>(
                        rng::sample<48>(pos.cast<Fixnum>(), rng::utility_state),
                        p));
                }
            });
        });
    });

    APP.camera()->shake(18);
}



void big_explosion_inv(const Vec2<Fixnum>& position)
{
    for (int i = 0; i < 1; ++i) {
        auto exp = APP.alloc_entity<Explosion>(
            rng::sample<48>(position, rng::utility_state));
        exp->seek_end();
        APP.effects().push(std::move(exp));
    }

    APP.on_timeout(milliseconds(90), [pos = position]() {
        APP.rumble().activate(milliseconds(390));

        for (int i = 0; i < 2; ++i) {
            auto exp = APP.alloc_entity<Explosion>(
                rng::sample<48>(pos, rng::utility_state));
            exp->seek_end();
            APP.effects().push(std::move(exp));
        }
        APP.on_timeout(milliseconds(90), [pos]() {
            for (int i = 0; i < 3; ++i) {
                auto exp = APP.alloc_entity<Explosion>(
                    rng::sample<32>(pos, rng::utility_state));
                exp->seek_end();
                APP.effects().push(std::move(exp));
            }
            APP.on_timeout(milliseconds(90), [pos]() {
                for (int i = 0; i < 4; ++i) {
                    auto exp = APP.alloc_entity<Explosion>(
                        rng::sample<18>(pos, rng::utility_state));
                    exp->seek_end();
                    APP.effects().push(std::move(exp));
                }
            });
        });
    });

    APP.camera()->shake(18);
}



} // namespace skyland
