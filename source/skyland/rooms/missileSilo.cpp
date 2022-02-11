#include "missileSilo.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland {



extern SharedVariable missile_damage;
SHARED_VARIABLE(missile_silo_reload_ms);



Sound missile_sound("missile");



void MissileSilo::format_description(StringBuffer<512>& buffer)
{
    auto secs = missile_silo_reload_ms / 1000;

    make_format(buffer,
                "A weapon for targeting the roof of an enemy fortress. "
                "Deals % damage every %.% seconds.",
                missile_damage,
                secs,
                (missile_silo_reload_ms / 100 - secs * 10));
}



MissileSilo::MissileSilo(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), position, 1000 * missile_silo_reload_ms)
{
}



void MissileSilo::fire(Platform& pfrm, App& app)
{
    auto island = other_island(app);

    Vec2<Float> target;

    auto room = island->get_room(*target_);
    if (room and not pfrm.network_peer().is_connected()) {
        // Note: if we use the center of a room as a target, we
        // have issues with multiplayer games, where a missile
        // targets a 2x2 room covered by 1x1 hull blocks for
        // example. Because the multiplayer coordinate system is
        // sort of mirrored over the y-axis, a missile aimed at
        // the border between two 1x1 blocks might hit the left
        // block in one game and the right block in another. So
        // missiles really should be constrained to columns for
        // multiplayer games. Just trying to explain the
        // network_peer().is_connected() check above.
        target = room->center();
    } else {
        auto origin = island->origin();
        origin.x += target_->x * 16 + 8;
        origin.y += target_->y * 16 + 8;
        target = origin;
    }

    if (not pfrm.network_peer().is_connected() and
        app.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<10>(target, rng::critical_state);
    }

    auto start = center();
    start.y -= 24;

    app.camera()->shake(6);

    auto m = app.alloc_entity<Missile>(
        pfrm, start, target, position().x, position().y, parent());

    missile_sound.play(pfrm, 3, milliseconds(400));

    if (m) {
        parent()->projectiles().push(std::move(m));
    }
}



Microseconds MissileSilo::reload() const
{
    return 1000 * missile_silo_reload_ms;
}



void MissileSilo::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::missile_silo_1;
    buffer[position().x][position().y + 1] = Tile::missile_silo_2;
}



void MissileSilo::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::missile_silo_1;
    buffer[position().x][position().y + 1] = Tile::missile_silo_2;
}



} // namespace skyland
