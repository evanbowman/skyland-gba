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
