#include "vendetta.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/vendettaBlast.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland {



extern Sound cannon_sound;



SHARED_VARIABLE(vendetta_reload_ms);
extern SharedVariable vendetta_blast_damage;



void Vendetta::format_description(StringBuffer<512>& buffer)
{
    auto secs = vendetta_reload_ms / 1000;

    make_format(
        buffer,
        "An unusual cannon-type weapon. Damage starts at %, and rises to "
        "% when health drops to 1/2, % when health drops below 1/4. "
        "%.% sec reload. Foundry needed.",
        vendetta_blast_damage,
        vendetta_blast_damage * 2,
        vendetta_blast_damage * 4,
        secs,
        (vendetta_reload_ms / 100 - secs * 10));

}



Vendetta::Vendetta(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), position, 1000 * vendetta_reload_ms)
{
}



void Vendetta::fire(Platform& pfrm, App& app)
{
    auto island = other_island(app);

    Vec2<Float> target;

    auto origin = island->origin();
    origin.x += target_->x * 16 + 8;
    origin.y += target_->y * 16 + 8;
    target = origin;

    app.camera().shake(4);

    auto start = center();

    if (island == &app.player_island()) {
        start.x -= 22;
    } else {
        start.x += 22;
    }

    if (not pfrm.network_peer().is_connected() and
        app.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(pfrm, 3);

    auto v = alloc_entity<VendettaBlast>(start, target, parent(), position());
    if (v) {
        if (health() < max_health() / 4) {
            v->set_variant(2);
        } else if (health() < max_health() / 2) {
            v->set_variant(1);
        }

        parent()->projectiles().push(std::move(v));
    }
}



Microseconds Vendetta::reload() const
{
    return 1000 * vendetta_reload_ms;
}



void Vendetta::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::vendetta_1;
    buffer[position().x + 1][position().y] = InteriorTile::vendetta_2;
}



void Vendetta::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::vendetta_1;
    buffer[position().x + 1][position().y] = Tile::vendetta_2;
}



} // namespace skyland
