#include "ionCannon.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/ionBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



extern SharedVariable ion_burst_damage;
SHARED_VARIABLE(ion_cannon_reload_ms);



void IonCannon::format_description(StringBuffer<512>& buffer)
{
    buffer += "Deals ion damage. Ion bursts pass harmlessly through most "
              "rooms, but deals ";
    buffer += stringify(ion_burst_damage);
    buffer += " damage every ";
    auto secs = ion_cannon_reload_ms / 1000;
    buffer += stringify(secs);
    buffer += ".";
    buffer += stringify((ion_cannon_reload_ms / 100 - secs * 10));
    buffer += "s to forcefields, reactors, etc.. Requires "
              "a workshop to build.";
}



IonCannon::IonCannon(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), size(), position, 1000 * ion_cannon_reload_ms)
{
}



void IonCannon::fire(Platform& pfrm, App& app)
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

    auto c =
        app.alloc_entity<IonBurst>(pfrm, start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }
}



Microseconds IonCannon::reload() const
{
    return 1000 * ion_cannon_reload_ms;
}



void IonCannon::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::particle_gun;
}



void IonCannon::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::particle_gun;
}



} // namespace skyland
