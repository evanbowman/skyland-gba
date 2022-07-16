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


#pragma once

#include "number/int.h"
#include "scene.hpp"
#include "systemString.hpp"



class Platform;



namespace skyland
{



class App;



}



namespace skyland::achievements
{



enum Achievement : u8 {
    // clang-format off
    none            = 0,
    builder         = 1,
    architect       = 2,
    architect_2     = 3,
    explorer        = 4,
    strategist      = 5,
    stronghold      = 6,
    dynamite        = 7,
    maestro_1       = 8,
    maestro_2       = 9,
    triage          = 10,
    banana_man      = 11,
    ancient_weapon  = 12,
    ship_of_theseus = 13,
    lemons          = 14,
    new_colossus    = 15,
    meltdown        = 16,
    completionist   = 17,
    mycelium        = 18,
    primitive       = 19,
    hero            = 20,
    count
    // clang-format on
};



// Inspects save data, and re-awards any achievements from a previous session.
void init(Platform& pfrm, App& app);



// If the library matches a new achievement, return the achievement.
Achievement update(Platform& pfrm, App& app);



// Raise an alert, forcibly unlocking an achievement.
void raise(Platform& pfrm, App& app, Achievement achievement);



// Re-lock the achievement. Really just needed for implementing rewind.
void lock(Platform& pfrm, App& app, Achievement achievement);



// update() or unlock() return an achievement, or true (respectively), call
// award to award the achievement item to the player.
void award(Platform& pfrm, App& app, Achievement achievement);



bool is_unlocked(App& app, Achievement achievement);



SystemString description(Achievement achievement);



SystemString name(Achievement achievement);



const char* reward(Achievement achievement);



} // namespace skyland::achievements
