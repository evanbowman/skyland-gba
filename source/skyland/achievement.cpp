////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "achievement.hpp"
#include "room_metatable.hpp"
#include "save.hpp"
#include "script/lisp.hpp"
#include "skyland.hpp"
#include "timeStreamEvent.hpp"



namespace skyland::achievements
{



static u8 challenge_count;



struct AchievementInfo
{
    SystemString name_;
    SystemString description_;
    const char* reward_;

    bool (*match_)();
    void (*award_)(bool);
};



static const AchievementInfo info[Achievement::count] = {
    {SystemString::empty,
     SystemString::empty,
     "none",
     []() { return false; },
     [](bool) {}},

    {SystemString::achievement_builder_name,
     SystemString::achievement_builder_description,
     "masonry",
     []() { return APP.player_island().rooms().size() > 10; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[builder].reward_), awarded);
     }},

    {SystemString::achievement_architect_name,
     SystemString::achievement_architect_description,
     "bridge",
     []() { return APP.player_island().rooms().size() > 20; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[architect].reward_), awarded);
     }},

    {SystemString::achievement_architect2_name,
     SystemString::achievement_architect2_description,
     "fountain",
     []() { return APP.player_island().rooms().size() > 20; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[architect_2].reward_), awarded);
     }},

    {SystemString::achievement_explorer_name,
     SystemString::achievement_explorer_description,
     "bell",
     []() { return APP.zone() > 1; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[explorer].reward_), awarded);
     }},

    {SystemString::achievement_strategist_name,
     SystemString::achievement_strategist_description,
     "statue",
     []() { return APP.zone() > 2; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[strategist].reward_), awarded);
     }},

    {SystemString::achievement_stronghold_name,
     SystemString::achievement_stronghold_description,
     "bronze-hull",
     []() { return APP.player_island().core_count() > 3; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[stronghold].reward_), awarded);
     }},

    {SystemString::achievement_dynamite_name,
     SystemString::achievement_dynamite_description,
     "dynamite",
     []() { return APP.gp_.challenge_flags_.get() not_eq 0; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[dynamite].reward_), awarded);
     }},

    {SystemString::achievement_maestro1_name,
     SystemString::achievement_maestro1_description,
     "speaker",
     []() { return count_ones(APP.gp_.challenge_flags_.get()) > 2; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[maestro_1].reward_), awarded);
     }},

    {SystemString::achievement_maestro2_name,
     SystemString::achievement_maestro2_description,
     "synth",
     []() { return count_ones(APP.gp_.challenge_flags_.get()) > 2; },
     [](bool awarded) {
         set_enabled(metaclass_index(info[maestro_2].reward_), awarded);
     }},

    {SystemString::achievement_triage_name,
     SystemString::achievement_triage_description,
     "dynamite-ii",
     []() {
         // invoked manually through achievements::raise().
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[triage].reward_), awarded);
     }},

    {SystemString::achievement_banana_man_name,
     SystemString::achievement_banana_man_description,
     "banana-plant",
     []() {
         // invoked manually through achievements::raise().
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[banana_man].reward_), awarded);
     }},

    {SystemString::achievement_end_of_line_name,
     SystemString::achievement_end_of_line_description,
     "spark-cannon",
     []() {
         if (APP.zone() == 4) {
             auto current_x = APP.world_graph()
                                  .nodes_[APP.current_world_location()]
                                  .coord_.x;

             for (auto& node : APP.world_graph().nodes_) {
                 if (node.coord_.x > current_x) {
                     return false;
                 }
             }

             return true;
         }
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[edge_of_world].reward_), awarded);
     }},

    {SystemString::achievement_ship_of_theseus_name,
     SystemString::achievement_ship_of_theseus_description,
     "mirror-hull",
     []() {
         // invoked manually through achievements::raise().
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[ship_of_theseus].reward_), awarded);
     }},

    {SystemString::achievement_lemons_name,
     SystemString::achievement_lemons_description,
     "lemon-tree",
     []() {
         // invoked manually through achievements::raise().
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[lemons].reward_), awarded);
     }},

    {SystemString::achievement_new_colossus_name,
     SystemString::achievement_new_colossus_description,
     "lady-liberty",
     []() {
         if (player_island().character_count() >= 7) {
             u8 count = 0;
             for (auto& room : player_island().rooms()) {
                 for (auto& chr : room->characters()) {
                     if (not chr->is_replicant()) {
                         ++count;
                     }
                 }
             }

             return count >= 7;
         }
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[new_colossus].reward_), awarded);
     }},

    {SystemString::achievement_meltdown_name,
     SystemString::achievement_meltdown_description,
     "radiator",
     []() {
         // invoked manually through achievements::raise().
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[meltdown].reward_), awarded);
     }},

    {SystemString::achievement_completionist_name,
     SystemString::achievement_completionist_description,
     "annihilator",
     []() {
         auto bc = count_ones(APP.gp_.challenge_flags_.get());
         if (bc < challenge_count) {
             return false;
         }

         return true;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[completionist].reward_), awarded);
     }},

    {SystemString::achievement_mycelium_name,
     SystemString::achievement_mycelium_description,
     "mycelium",
     []() {
         // invoked manually through achievements::raise().
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[mycelium].reward_), awarded);
     }},

    {SystemString::achievement_primitive_name,
     SystemString::achievement_primitive_description,
     "windmill",
     []() {
         return APP.zone() > 2 and
                not(APP.persistent_data().state_flags_.get() &
                    PersistentData::workshop_built);
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[primitive].reward_), awarded);
     }},

    {SystemString::achievement_hero_name,
     SystemString::achievement_hero_description,
     "weather-engine",
     []() {
         // invoked manually through achievements::raise().
         return false;
     },
     [](bool awarded) {
         set_enabled(metaclass_index(info[hero].reward_), awarded);
     }},

    // {SystemString::achievement_pacifist_name,
    //  SystemString::achievement_pacifist_description,
    //  "mind-control",
    //  []() {
    //      return APP.zone() > 3 and
    //          not(APP.persistent_data().state_flags_.get() &
    //              PersistentData::opponent_crew_died);
    //  },
    //  []( bool awarded){
    //      set_enabled(metaclass_index(info[raid].reward_), awarded);
    //  }}
};



void init()
{
    auto flags = APP.gp_.achievement_flags_;

    // Required for an achievement.
    challenge_count =
        lisp::length(APP.invoke_script("/scripts/challenges/index.lisp"));

    for (int i = 0; i < Achievement::count; ++i) {
        const u64 flag = 1 << i;

        if (flags.get() & flag) {
            info[i].award_(true);
        }
    }
}



static Bitvector<Achievement::count> raised_achievements;



void raise(Achievement achievement)
{
    auto& flags = APP.gp_.achievement_flags_;
    const u64 flag = 1 << achievement;

    if (flags.get() & flag) {
        // We've already unlocked the achievement, silently ignore.
        return;
    }

    raised_achievements.set(achievement, true);
}



// For efficiency/scalability, only check one achievement per update call. Store
// the last checked achievement in a variable. Round-robin through the
// achievements, one check per frame.
static Achievement last_achievement = (Achievement)((int)Achievement::none + 1);



Achievement update()
{
    auto check_achievement = last_achievement + 1;
    if (check_achievement == Achievement::count) {
        check_achievement = (Achievement)((int)Achievement::none + 1);
    }

    last_achievement = (Achievement)check_achievement;

    auto& flags = APP.gp_.achievement_flags_;

    const u64 flag = 1 << check_achievement;

    static_assert(Achievement::count < (sizeof(flag) * 8),
                  "More than 64 achievements, i.e. achievement bit does not "
                  "fit in a u64.");

    if (not(flags.get() & flag)) {
        if (raised_achievements.get(check_achievement) or
            info[check_achievement].match_()) {
            raised_achievements.set(check_achievement, false);
            flags.set(flags.get() | flag);
            save::store_global_data(APP.gp_);
            return static_cast<Achievement>(check_achievement);
        }
    }

    return Achievement::none;
}



void lock(Achievement achievement)
{
    auto& flags = APP.gp_.achievement_flags_;
    const u64 flag = 1 << achievement;

    flags.set(flags.get() & ~flag);

    save::store_global_data(APP.gp_);

    info[achievement].award_(false);
}



void award(Achievement achievement)
{
    info[achievement].award_(true);

    time_stream::event::Achievement e;
    e.which_ = achievement;
    APP.time_stream().push(APP.level_timer(), e);
}



bool is_unlocked(Achievement achievement)
{
    auto& flags = APP.gp_.achievement_flags_;
    const u64 flag = 1 << achievement;

    return flags.get() & flag;
}



SystemString description(Achievement achievement)
{
    return info[achievement].description_;
}



SystemString name(Achievement achievement)
{
    return info[achievement].name_;
}



const char* reward(Achievement achievement)
{
    return info[achievement].reward_;
}



} // namespace skyland::achievements
