#include "arcGun.hpp"
#include "localization.hpp"
#include "skyland/entity/projectile/arcBolt.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland {



SHARED_VARIABLE(arc_gun_reload_ms);
extern Sound cannon_sound;
extern SharedVariable arcbolt_damage;



void ArcGun::format_description(StringBuffer<512>& buffer)
{
    buffer += "Deals ";
    buffer += to_string<10>(arcbolt_damage);
    buffer += " damage every ";
    auto secs = arc_gun_reload_ms / 1000;
    buffer += to_string<10>(secs);
    buffer += ".";
    buffer += to_string<10>((arc_gun_reload_ms / 100 - secs * 10));
    buffer +=
        " seconds, but unlike other weapons, damage chains"
        " to all neighboring structures of the same type. Requires a foundry.";
}



ArcGun::ArcGun(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), size(), position, 1000 * arc_gun_reload_ms)
{
}



void ArcGun::fire(Platform& pfrm, App& app)
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

    auto ab =
        alloc_entity<ArcBolt>(start, target, parent(), position());
    if (ab) {
        parent()->projectiles().push(std::move(ab));
    }
}



Microseconds ArcGun::reload() const
{
    return 1000 * arc_gun_reload_ms;
}



void ArcGun::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::arc_gun;
}



void ArcGun::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::arc_gun;
}



} // namespace skyland
