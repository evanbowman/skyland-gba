////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "nemesis.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/nemesisBlast.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern Sound cannon_sound;



SHARED_VARIABLE(nemesis_reload_ms);
extern SharedVariable nemesis_blast_damage;



void Nemesis::format_description(StringBuffer<512>& buffer)
{
    auto secs = nemesis_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_nemesis)->c_str(),
                nemesis_blast_damage,
                nemesis_blast_damage * 2,
                nemesis_blast_damage * 4,
                secs,
                (nemesis_reload_ms / 100 - secs * 10));
}



Nemesis::Nemesis(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * nemesis_reload_ms)
{
}



void Nemesis::fire()
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
        start.x -= 22.0_fixed;
    } else {
        start.x += 22.0_fixed;
    }

    if (not PLATFORM.network_peer().is_connected() and
        APP.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(3);

    auto v =
        APP.alloc_entity<NemesisBlast>(start, target, parent(), position());
    if (v) {
        if (health() < max_health() / 4) {
            v->set_variant(2);
        } else if (health() < max_health() / 2) {
            v->set_variant(1);
        }

        parent()->projectiles().push(std::move(v));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }
}



Time Nemesis::reload_impl() const
{
    return 1000 * nemesis_reload_ms;
}



void Nemesis::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::nemesis_1;
    buffer[position().x + 1][position().y] = InteriorTile::nemesis_2;
}



void Nemesis::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::nemesis_1;
    buffer[position().x + 1][position().y] = Tile::nemesis_2;
}



} // namespace skyland
