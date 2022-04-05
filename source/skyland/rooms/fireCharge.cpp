#include "fireCharge.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/fireBolt.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(fire_charge_reload_ms);



extern Sound cannon_sound;



void FireCharge::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    // auto secs = cannon_reload_ms / 1000;

    // make_format(buffer,
    //             SYSTR(description_cannon)->c_str(),
    //             cannonball_damage,
    //             secs,
    //             (cannon_reload_ms / 100 - secs * 10));
}



FireCharge::FireCharge(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), position, 1000 * fire_charge_reload_ms)
{
}



void FireCharge::fire(Platform& pfrm, App& app)
{
    auto island = other_island(app);

    Vec2<Float> target;

    auto origin = island->origin();
    origin.x += target_->x * 16 + 8;
    origin.y += target_->y * 16 + 8;
    target = origin;

    app.camera()->shake(4);

    auto start = center();

    // This just makes it a bit less likely for cannonballs to
    // run into the player's own buildings, especially around
    // corners.
    if (island == &app.player_island()) {
        start.x -= 23;
    } else {
        start.x += 23;
    }

    if (not pfrm.network_peer().is_connected() and
        app.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(pfrm, 3);

    auto c =
        app.alloc_entity<FireBolt>(pfrm, start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }
}



Microseconds FireCharge::reload() const
{
    return 1000 * fire_charge_reload_ms;
}



void FireCharge::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::fire_charge_1;
    buffer[position().x + 1][position().y] = InteriorTile::fire_charge_2;
}



void FireCharge::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::fire_charge_1;
    buffer[position().x + 1][position().y] = Tile::fire_charge_2;
}



} // namespace skyland
