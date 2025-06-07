////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "sylphCannon.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(sylph_cannon_reload_ms);
extern SharedVariable cannonball_damage;
extern SharedVariable sylph_cannon_damage_percent;



extern Sound cannon_sound;



void SylphCannon::format_description(StringBuffer<512>& buffer)
{
    make_format(buffer,
                SYSTR(description_sylph_cannon)->c_str(),
                sylph_cannon_damage_percent,
                "%");
}



SylphCannon::SylphCannon(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * sylph_cannon_reload_ms)
{
}



void SylphCannon::fire()
{
    auto island = other_island();

    Vec2<Fixnum> target;

    auto origin = island->origin();
    origin.x += Fixnum::from_integer(get_target()->x * 16 + 8);
    origin.y += Fixnum::from_integer(get_target()->y * 16 + 8);
    target = origin;

    APP.camera()->shake(4);

    auto start = center();

    // This just makes it a bit less likely for cannonballs to
    // run into the player's own buildings, especially around
    // corners.
    if (is_player_island(island)) {
        start.x -= 6.0_fixed;
    } else {
        start.x += 6.0_fixed;
    }

    if (not PLATFORM.network_peer().is_connected() and
        APP.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<4>(target, rng::critical_state);
    }

    cannon_sound.play(3);

    auto c = APP.alloc_entity<Cannonball>(start, target, parent(), position());
    if (c) {
        c->set_variant(2);
        parent()->projectiles().push(std::move(c));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }
}



Time SylphCannon::reload_impl() const
{
    return 1000 * sylph_cannon_reload_ms;
}



void SylphCannon::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::sylph_cannon;
}



void SylphCannon::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::sylph_cannon;
}



} // namespace skyland
