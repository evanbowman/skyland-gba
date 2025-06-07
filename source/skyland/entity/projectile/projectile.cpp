////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "projectile.hpp"
#include "skyland/scene/constructionScene.hpp"



namespace skyland
{



void Projectile::destroy_out_of_bounds(Island* target)
{
    auto t_y = target->origin().y.as_integer();
    auto max_y = t_y + 16 * 16 + 32;
    auto min_y = t_y + construction_zone_min_y * 16;
    int max_x = 9999999;
    int min_x = -9999999;
    if (target == &player_island()) {
        // If we're shooting at the player's island, the projectile moves
        // leftwards, and we care about the min bound.
        min_x = target->origin().x.as_integer() - 32;
    } else {
        // Otherwise, we need to check the max bound.
        max_x = target->origin().x.as_integer() +
                16 * target->terrain().size() + 32;
    }

    auto pos = sprite_.get_position();

    if (pos.y.as_integer() > max_y or pos.y.as_integer() < min_y or
        pos.x.as_integer() > max_x or pos.x.as_integer() < min_x) {
        this->destroy(pos.y.as_integer() > min_y);
        PLATFORM.speaker().play_sound("explosion1", 2);
    }
}



void Projectile::destroy(bool explosion)
{
    Platform::fatal("unimplemented destroy method!");
}



} // namespace skyland
