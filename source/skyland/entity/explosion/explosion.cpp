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
