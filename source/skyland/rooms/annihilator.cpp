////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "annihilator.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/antimatter.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern Sound ion_cannon_sound;



void Annihilator::format_description(StringBuffer<512>& buffer)
{
    buffer = SYSTR(description_annihilator)->c_str();
}



Annihilator::Annihilator(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * 3000)
{
}



void Annihilator::fire()
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

    ion_cannon_sound.play(3);

    auto c = APP.alloc_entity<Antimatter>(start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }
}



Time Annihilator::reload_impl() const
{
    return 1000 * 3000;
}



void Annihilator::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::annihilator_1;
    buffer[position().x][position().y + 1] = InteriorTile::annihilator_2;
}



void Annihilator::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::annihilator_1;
    buffer[position().x][position().y + 1] = Tile::annihilator_2;
}



} // namespace skyland
