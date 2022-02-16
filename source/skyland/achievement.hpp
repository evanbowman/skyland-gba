#pragma once

#include "number/int.h"
#include "scene.hpp"



class Platform;



namespace skyland {



class App;



}



namespace skyland::achievements {



enum Achievement : u8 {
    none,
    builder,
    architect,
    explorer,
    strategist,
    stronghold,
    maestro_1,
    maestro_2,
    triage,
    ancient_weapon,
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



const char* description(Achievement achievement);



const char* name(Achievement achievement);



const char* reward(Achievement achievement);



} // namespace skyland::achievements
