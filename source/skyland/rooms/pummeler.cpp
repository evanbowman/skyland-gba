////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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

    if (is_player_island(island)) {
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



Time Pummeler::reload() const
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
