////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "warhead.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern Sound missile_sound;
SHARED_VARIABLE(warhead_reload_ms);



void Warhead::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_warhead)->c_str();
}



static auto reload_time()
{
    return 1000 * warhead_reload_ms + seconds(3);
}



Warhead::Warhead(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, reload_time())
{
}



void Warhead::fire()
{
    auto island = other_island();

    Vec2<Fixnum> target;

    auto room = island->get_room(*get_target());
    if (room and not PLATFORM.network_peer().is_connected()) {
        // Note: if we use the center of a room as a target, we
        // have issues with multiplayer games, where a missile
        // targets a 2x2 room covered by 1x1 hull blocks for
        // example. Because the multiplayer coordinate system is
        // sort of mirrored over the y-axis, a missile aimed at
        // the border between two 1x1 blocks might hit the left
        // block in one game and the right block in another. So
        // missiles really should be constrained to columns for
        // multiplayer games. Just trying to explain the
        // network_peer().is_connected() check above.
        target = room->center();
    } else {
        auto origin = island->origin();
        origin.x += Fixnum::from_integer(get_target()->x * 16 + 8);
        origin.y += Fixnum::from_integer(get_target()->y * 16 + 8);
        target = origin;
    }

    auto start = center();
    start.y -= 24.0_fixed;

    APP.camera()->shake(6);

    auto m = APP.alloc_entity<AtomicMissile>(
        start, target, position().x, position().y, parent());

    missile_sound.play(3, milliseconds(400));

    if (m) {
        parent()->projectiles().push(std::move(m));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }

    apply_damage(health_upper_limit());
}



Time Warhead::reload_impl() const
{
    return reload_time();
}



void Warhead::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::warhead_1;
    buffer[position().x][position().y + 1] = InteriorTile::warhead_2;
}



void Warhead::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::warhead_1;
    buffer[position().x][position().y + 1] = Tile::warhead_2;
}



} // namespace skyland
