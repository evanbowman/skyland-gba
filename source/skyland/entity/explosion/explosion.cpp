#include "explosion.hpp"
#include "number/random.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void medium_explosion(Platform& pfrm, App& app, const Vec2<Float>& position)
{
    app.effects().push(
        alloc_entity<Explosion>(rng::sample<18>(position, rng::utility_state)));

    app.on_timeout(
        pfrm, milliseconds(60), [pos = position](Platform& pf, App& app) {
            app.rumble().activate(pf, milliseconds(200));
            app.effects().push(alloc_entity<Explosion>(
                rng::sample<18>(pos, rng::utility_state)));

            app.on_timeout(
                pf, milliseconds(120), [pos = pos](Platform&, App& app) {
                    app.effects().push(alloc_entity<Explosion>(
                        rng::sample<18>(pos, rng::utility_state)));
                });
        });
}



void big_explosion(Platform& pfrm, App& app, const Vec2<Float>& position)
{
    for (int i = 0; i < 4; ++i) {
        app.effects().push(alloc_entity<Explosion>(
            rng::sample<18>(position, rng::utility_state)));
    }

    app.on_timeout(
        pfrm, milliseconds(90), [pos = position](Platform& pf, App& app) {
            app.rumble().activate(pf, milliseconds(390));

            for (int i = 0; i < 3; ++i) {
                app.effects().push(alloc_entity<Explosion>(
                    rng::sample<32>(pos, rng::utility_state)));
            }
            app.on_timeout(pf, milliseconds(90), [pos](Platform& pf, App& app) {
                for (int i = 0; i < 2; ++i) {
                    app.effects().push(alloc_entity<Explosion>(
                        rng::sample<48>(pos, rng::utility_state)));
                }
                app.on_timeout(
                    pf, milliseconds(90), [pos](Platform&, App& app) {
                        for (int i = 0; i < 1; ++i) {
                            app.effects().push(alloc_entity<Explosion>(
                                rng::sample<48>(pos, rng::utility_state)));
                        }
                    });
            });
        });

    app.camera().shake(18);
}



} // namespace skyland
