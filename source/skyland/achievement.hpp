////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    edge_of_world   = 12,
    ship_of_theseus = 13,
    lemons          = 14,
    new_colossus    = 15,
    meltdown        = 16,
    completionist   = 17,
    mycelium        = 18,
    primitive       = 19,
    hero            = 20,
    // pacifist        = 23,
    count
    // clang-format on
};



// Inspects save data, and re-awards any achievements from a previous session.
void init();



// If the library matches a new achievement, return the achievement.
Achievement update();



// Raise an alert, forcibly unlocking an achievement.
void raise(Achievement achievement);



// Re-lock the achievement. Really just needed for implementing rewind.
void lock(Achievement achievement);



// update() or unlock() return an achievement, or true (respectively), call
// award to award the achievement item to the player.
void award(Achievement achievement);



bool is_unlocked(Achievement achievement);



SystemString description(Achievement achievement);



SystemString name(Achievement achievement);



const char* reward(Achievement achievement);



} // namespace skyland::achievements
