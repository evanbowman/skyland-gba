////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "arcGun.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/arcBolt.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(arc_gun_reload_ms);
extern Sound cannon_sound;
extern SharedVariable arcbolt_damage;



void ArcGun::format_description(StringBuffer<512>& buffer)
{
    auto secs = arc_gun_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_arc_gun)->c_str(),
                arcbolt_damage,
                secs,
                arc_gun_reload_ms / 100 - secs * 10);
}



ArcGun::ArcGun(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * arc_gun_reload_ms)
{
}



void ArcGun::fire()
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
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(3);

    auto ab = APP.alloc_entity<ArcBolt>(start, target, parent(), position());
    if (ab) {
        parent()->projectiles().push(std::move(ab));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }
}



Time ArcGun::reload_impl() const
{
    return 1000 * arc_gun_reload_ms;
}



void ArcGun::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::arc_gun;
}



void ArcGun::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::arc_gun;
}



} // namespace skyland
