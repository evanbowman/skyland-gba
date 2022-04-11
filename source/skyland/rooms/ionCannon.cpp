////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "ionCannon.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/ionBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern SharedVariable ion_burst_damage;
SHARED_VARIABLE(ion_cannon_reload_ms);



void IonCannon::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    auto secs = ion_cannon_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_ion_cannon)->c_str(),
                ion_burst_damage,
                secs,
                ion_cannon_reload_ms / 100 - secs * 10);
}



IonCannon::IonCannon(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * ion_cannon_reload_ms)
{
}



void IonCannon::fire(Platform& pfrm, App& app)
{
    auto island = other_island(app);

    Vec2<Fixnum> target;

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



void IonCannon::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::particle_gun;
}



void IonCannon::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::particle_gun;
}



} // namespace skyland
