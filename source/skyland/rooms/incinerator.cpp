////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    origin.x += Fixnum::from_integer(get_target()->x * 16 + 8);
    origin.y += Fixnum::from_integer(get_target()->y * 16 + 8);
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



Time Incinerator::reload_impl() const
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



void Incinerator::finalize_weapon()
{
    if (health() <= 0) {
        auto pos = center();
        pos.y += 8.0_fixed;
        ExploSpawner::create(pos);
        pos.y -= 16.0_fixed;
        ExploSpawner::create(pos);
    }
}



} // namespace skyland
