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


#include "annihilator.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/antimatter.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern Sound ion_cannon_sound;



void Annihilator::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
}



Annihilator::Annihilator(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * 3000)
{
}



void Annihilator::fire(Platform& pfrm, App& app)
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

    ion_cannon_sound.play(pfrm, 3);

    auto c =
        app.alloc_entity<Antimatter>(pfrm, start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }
}



Microseconds Annihilator::reload() const
{
    return 1000 * 3000;
}



void Annihilator::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::annihilator_1;
    buffer[position().x][position().y + 1] = InteriorTile::annihilator_2;
}



void Annihilator::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::annihilator_1;
    buffer[position().x][position().y + 1] = Tile::annihilator_2;
}



} // namespace skyland
