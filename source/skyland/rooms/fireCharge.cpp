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


#include "fireCharge.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
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
    buffer += SYSTR(description_fire_charge)->c_str();
}



FireCharge::FireCharge(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * fire_charge_reload_ms)
{
}



void FireCharge::fire(Platform& pfrm, App& app)
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
        start.x -= 23;
        target.x += 14;
    } else {
        start.x += 23;
        target.x -= 8;
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

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        app.effects().push(std::move(e));
    }
}



Microseconds FireCharge::reload() const
{
    return 1000 * fire_charge_reload_ms;
}



void FireCharge::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::fire_charge_1;
    buffer[position().x + 1][position().y] = InteriorTile::fire_charge_2;
}



void FireCharge::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::fire_charge_1;
    buffer[position().x + 1][position().y] = Tile::fire_charge_2;
}



} // namespace skyland
