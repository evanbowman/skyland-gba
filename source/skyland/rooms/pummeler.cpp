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


#include "pummeler.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/curveshot.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Pummeler::Pummeler(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * milliseconds(3))
{
}



void Pummeler::fire()
{
    auto island = other_island();

    Vec2<Fixnum> target;

    auto origin = island->origin();
    origin.x += target_->x * 16 + 8;
    origin.y += target_->y * 16 + 8;
    target = origin;

    APP.camera()->shake(4);

    auto start = center();

    if (island == &APP.player_island()) {
        start.x -= 6;
    } else {
        start.x += 6;
    }

    if (not PLATFORM.network_peer().is_connected() and
        APP.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    // auto c = APP.alloc_entity<Curveshot>(
    //     start, target, parent(), other_island(), position(), *target_);
    // if (c) {
    //     parent()->projectiles().push(std::move(c));
    // }
}



Microseconds Pummeler::reload() const
{
    return 1000 * milliseconds(3);
}



void Pummeler::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::cannon_1;
}



void Pummeler::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::cannon_1;
}



} // namespace skyland
