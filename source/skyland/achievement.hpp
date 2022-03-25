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
    none,
    builder,
    architect,
    architect_2,
    explorer,
    strategist,
    stronghold,
    dynamite,
    maestro_1,
    maestro_2,
    triage,
    banana_man,
    ancient_weapon,
    ship_of_theseus,
    lemons,
    new_colossus,
    meltdown,
    count
};



// Inspects save data, and re-awards any achievements from a previous session.
void init(Platform& pfrm, App& app);



// If the library matches a new achievement, return the achievement.
Achievement update(Platform& pfrm, App& app);



// Manually unlock an achievement. Returns true if the achievement was locked.
bool unlock(Platform& pfrm, App& app, Achievement achievement);


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
