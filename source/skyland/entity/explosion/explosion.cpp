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



void medium_explosion(Platform& pfrm, App& app, const Vec2<Fixnum>& position)
{
    app.effects().push(app.alloc_entity<Explosion>(
        pfrm, rng::sample<18>(position, rng::utility_state)));

    app.on_timeout(
        pfrm, milliseconds(60), [pos = position](Platform& pf, App& app) {
            app.rumble().activate(pf, milliseconds(200));
            app.effects().push(app.alloc_entity<Explosion>(
                pf, rng::sample<18>(pos, rng::utility_state)));

            app.on_timeout(
                pf, milliseconds(120), [pos = pos](Platform& pf, App& app) {
                    app.effects().push(app.alloc_entity<Explosion>(
                        pf, rng::sample<18>(pos, rng::utility_state)));
                });
        });
}



void medium_explosion_inv(Platform& pfrm,
                          App& app,
                          const Vec2<Fixnum>& position)
{
    auto first = app.alloc_entity<Explosion>(
        pfrm, rng::sample<18>(position, rng::utility_state));
    first->seek_end();
    app.effects().push(std::move(first));

    app.on_timeout(
        pfrm, milliseconds(60), [pos = position](Platform& pf, App& app) {
            app.rumble().activate(pf, milliseconds(200));
            auto exp = app.alloc_entity<Explosion>(
                pf, rng::sample<18>(pos, rng::utility_state));
            exp->seek_end();
            app.effects().push(std::move(exp));

            app.on_timeout(
                pf, milliseconds(120), [pos = pos](Platform& pf, App& app) {
                    auto exp = app.alloc_entity<Explosion>(
                        pf, rng::sample<18>(pos, rng::utility_state));
                    exp->seek_end();
                    app.effects().push(std::move(exp));
                });
        });
}



void big_explosion(Platform& pfrm,
                   App& app,
                   const Vec2<Fixnum>& position,
                   int draw_priority)
{
    for (int i = 0; i < 4; ++i) {
        app.effects().push(app.alloc_entity<Explosion>(pfrm, rng::sample<18>(position, rng::utility_state), draw_priority));
    }

    int p = draw_priority;
    auto ipos = ivec(position);

    app.on_timeout(
                   pfrm, milliseconds(90), [pos = ipos, p](Platform& pf, App& app) {
            app.rumble().activate(pf, milliseconds(390));

            for (int i = 0; i < 3; ++i) {
                app.effects().push(app.alloc_entity<Explosion>(
                                                               pf, rng::sample<32>(pos.cast<Fixnum>(), rng::utility_state), p));
            }
            app.on_timeout(pf, milliseconds(90), [pos, p](Platform& pf, App& app) {
                for (int i = 0; i < 2; ++i) {
                    app.effects().push(app.alloc_entity<Explosion>(
                                                                   pf, rng::sample<48>(pos.cast<Fixnum>(), rng::utility_state), p));
                }
                app.on_timeout(
                               pf, milliseconds(90), [pos, p](Platform& pf, App& app) {
                        for (int i = 0; i < 1; ++i) {
                            app.effects().push(app.alloc_entity<Explosion>(
                                                                           pf, rng::sample<48>(pos.cast<Fixnum>(), rng::utility_state), p));
                        }
                    });
            });
        });

    app.camera()->shake(18);
}



void big_explosion_inv(Platform& pfrm, App& app, const Vec2<Fixnum>& position)
{
    for (int i = 0; i < 1; ++i) {
        auto exp = app.alloc_entity<Explosion>(
            pfrm, rng::sample<48>(position, rng::utility_state));
        exp->seek_end();
        app.effects().push(std::move(exp));
    }

    app.on_timeout(
        pfrm, milliseconds(90), [pos = position](Platform& pf, App& app) {
            app.rumble().activate(pf, milliseconds(390));

            for (int i = 0; i < 2; ++i) {
                auto exp = app.alloc_entity<Explosion>(
                    pf, rng::sample<48>(pos, rng::utility_state));
                exp->seek_end();
                app.effects().push(std::move(exp));
            }
            app.on_timeout(pf, milliseconds(90), [pos](Platform& pf, App& app) {
                for (int i = 0; i < 3; ++i) {
                    auto exp = app.alloc_entity<Explosion>(
                        pf, rng::sample<32>(pos, rng::utility_state));
                    exp->seek_end();
                    app.effects().push(std::move(exp));
                }
                app.on_timeout(
                    pf, milliseconds(90), [pos](Platform& pf, App& app) {
                        for (int i = 0; i < 4; ++i) {
                            auto exp = app.alloc_entity<Explosion>(
                                pf, rng::sample<18>(pos, rng::utility_state));
                            exp->seek_end();
                            app.effects().push(std::move(exp));
                        }
                    });
            });
        });

    app.camera()->shake(18);
}



} // namespace skyland
