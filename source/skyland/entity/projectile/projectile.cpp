////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
