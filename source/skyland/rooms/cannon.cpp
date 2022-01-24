#include "cannon.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland {



extern SharedVariable cannonball_damage;
SHARED_VARIABLE(cannon_reload_ms);



Sound cannon_sound("cannon");



void Cannon::format_description(StringBuffer<512>& buffer)
{
    buffer += "A reliable artillery weapon. Deals ";
    buffer += stringify(cannonball_damage);
    buffer += " damage every ";
    auto secs = cannon_reload_ms / 1000;
    buffer += stringify(secs);
    buffer += ".";
    buffer += stringify((cannon_reload_ms / 100 - secs * 10));
    buffer += " seconds.";
}



Cannon::Cannon(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), size(), position, 1000 * cannon_reload_ms)
{
}



void Cannon::fire(Platform& pfrm, App& app)
{
    auto island = other_island(app);

    Vec2<Float> target;

    auto origin = island->origin();
    origin.x += target_->x * 16 + 8;
    origin.y += target_->y * 16 + 8;
    target = origin;

    app.camera().shake(4);

    auto start = center();

    // This just makes it a bit less likely for cannonballs to
    // run into the player's own buildings, especially around
    // corners.
    if (island == &app.player_island()) {
        start.x -= 6;
    } else {
        start.x += 6;
    }

    if (not pfrm.network_peer().is_connected() and
        app.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(pfrm, 3);

    auto c = alloc_entity<Cannonball>(start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }
}



Microseconds Cannon::reload() const
{
    return 1000 * cannon_reload_ms;
}



void Cannon::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::cannon_1;
}



void Cannon::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::cannon_1;
}



} // namespace skyland
