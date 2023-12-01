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


#include "incinerator.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/incineratorBolt.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(incinerator_reload_ms);



extern Sound cannon_sound;



void Incinerator::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_incinerator)->c_str();
}



Incinerator::Incinerator(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * incinerator_reload_ms)
{
}



void Incinerator::fire()
{
    auto island = other_island();

    Vec2<Fixnum> target;

    auto origin = island->origin();
    origin.x += Fixnum::from_integer(target_->x * 16 + 8);
    origin.y += Fixnum::from_integer(target_->y * 16 + 8);
    target = origin;

    APP.camera()->shake(4);

    auto start = center();

    if (is_player_island(island)) {
        start.x -= 16.0_fixed;
    } else {
        start.x += 16.0_fixed;
    }
    start.y -= 3.0_fixed;

    if (not PLATFORM.network_peer().is_connected() and
        APP.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<4>(target, rng::critical_state);
    }

    cannon_sound.play(3);

    using Emit = IncineratorBolt;
    auto c = APP.alloc_entity<Emit>(start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }
}



Microseconds Incinerator::reload() const
{
    return 1000 * incinerator_reload_ms;
}



void Incinerator::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::incinerator_1;
    buffer[position().x + 1][position().y] = Tile::incinerator_2;
    buffer[position().x][position().y + 1] = Tile::incinerator_3;
    buffer[position().x + 1][position().y + 1] = Tile::incinerator_4;
}



void Incinerator::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::incinerator_1;
    buffer[position().x + 1][position().y] = Tile::incinerator_2;
    buffer[position().x][position().y + 1] = Tile::incinerator_3;
    buffer[position().x + 1][position().y + 1] = Tile::incinerator_4;
}



void Incinerator::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        auto pos = center();
        pos.y += 8.0_fixed;
        ExploSpawner::create(pos);
        pos.y -= 16.0_fixed;
        ExploSpawner::create(pos);
    }
}



} // namespace skyland
