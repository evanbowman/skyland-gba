////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "flakGun.hpp"
#include "globals.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(flak_gun_reload_ms);



extern Sound cannon_sound;



void FlakGun::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_flak_gun)->c_str();
}



FlakGun::FlakGun(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), position, 1000 * flak_gun_reload_ms)
{
}



void FlakGun::fire(Platform& pfrm, App& app)
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
        start.x -= 22;
    } else {
        start.x += 22;
    }

    if (not pfrm.network_peer().is_connected() and
        app.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(pfrm, 3);

    auto c = app.alloc_entity<Flak>(pfrm, start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }
}



Microseconds FlakGun::reload() const
{
    return 1000 * flak_gun_reload_ms;
}



void FlakGun::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::flak_gun_1;
    buffer[position().x + 1][position().y] = InteriorTile::flak_gun_2;
}



void FlakGun::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::flak_gun_1;
    buffer[position().x + 1][position().y] = Tile::flak_gun_2;
}



} // namespace skyland
