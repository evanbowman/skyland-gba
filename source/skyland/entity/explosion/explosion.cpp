#include "explosion.hpp"
#include "number/random.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void medium_explosion(Platform& pfrm, App& app, const Vec2<Float>& position)
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



void medium_explosion_inv(Platform& pfrm, App& app, const Vec2<Float>& position)
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



void big_explosion(Platform& pfrm, App& app, const Vec2<Float>& position)
{
    for (int i = 0; i < 4; ++i) {
        app.effects().push(app.alloc_entity<Explosion>(
            pfrm, rng::sample<18>(position, rng::utility_state)));
    }

    app.on_timeout(
        pfrm, milliseconds(90), [pos = position](Platform& pf, App& app) {
            app.rumble().activate(pf, milliseconds(390));

            for (int i = 0; i < 3; ++i) {
                app.effects().push(app.alloc_entity<Explosion>(
                    pf, rng::sample<32>(pos, rng::utility_state)));
            }
            app.on_timeout(pf, milliseconds(90), [pos](Platform& pf, App& app) {
                for (int i = 0; i < 2; ++i) {
                    app.effects().push(app.alloc_entity<Explosion>(
                        pf, rng::sample<48>(pos, rng::utility_state)));
                }
                app.on_timeout(
                    pf, milliseconds(90), [pos](Platform& pf, App& app) {
                        for (int i = 0; i < 1; ++i) {
                            app.effects().push(app.alloc_entity<Explosion>(
                                pf, rng::sample<48>(pos, rng::utility_state)));
                        }
                    });
            });
        });

    app.camera()->shake(18);
}



void big_explosion_inv(Platform& pfrm, App& app, const Vec2<Float>& position)
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
