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


#include "achievement.hpp"
#include "room_metatable.hpp"
#include "save.hpp"
#include "skyland.hpp"
#include "timeStreamEvent.hpp"



namespace skyland::achievements
{



struct AchievementInfo
{
    SystemString name_;
    SystemString description_;
    const char* reward_;

    bool (*match_)(Platform&, App&);
    void (*award_)(Platform&, App&, bool);
};



static const AchievementInfo info[Achievement::count] = {
    {SystemString::empty,
     SystemString::empty,
     "none",
     [](Platform&, App&) { return false; },
     [](Platform&, App&, bool) {}},

    {SystemString::achievement_builder_name,
     SystemString::achievement_builder_description,
     "masonry",
     [](Platform&, App& app) {
         return app.player_island().rooms().size() > 10;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[builder].reward_), awarded);
     }},

    {SystemString::achievement_architect_name,
     SystemString::achievement_architect_description,
     "bridge",
     [](Platform&, App& app) {
         return app.player_island().rooms().size() > 20;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[architect].reward_), awarded);
     }},

    {SystemString::achievement_architect2_name,
     SystemString::achievement_architect2_description,
     "fountain",
     [](Platform&, App& app) {
         return app.player_island().rooms().size() > 20;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[architect_2].reward_), awarded);
     }},

    {SystemString::achievement_explorer_name,
     SystemString::achievement_explorer_description,
     "coconut-palm",
     [](Platform&, App& app) { return app.zone() > 1; },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[explorer].reward_), awarded);
     }},

    {SystemString::achievement_strategist_name,
     SystemString::achievement_strategist_description,
     "statue",
     [](Platform&, App& app) { return app.zone() > 2; },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[strategist].reward_), awarded);
     }},

    {SystemString::achievement_stronghold_name,
     SystemString::achievement_stronghold_description,
     "bronze-hull",
     [](Platform&, App& app) { return app.player_island().core_count() > 3; },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[stronghold].reward_), awarded);
     }},

    {SystemString::achievement_dynamite_name,
     SystemString::achievement_dynamite_description,
     "dynamite",
     [](Platform&, App& app) {
         return app.gp_.challenge_flags_.get() not_eq 0;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[dynamite].reward_), awarded);
     }},

    {SystemString::achievement_maestro1_name,
     SystemString::achievement_maestro1_description,
     "speaker",
     [](Platform&, App& app) {
         return count_ones(app.gp_.challenge_flags_.get()) > 2;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[maestro_1].reward_), awarded);
     }},

    {SystemString::achievement_maestro2_name,
     SystemString::achievement_maestro2_description,
     "synth",
     [](Platform&, App& app) {
         return count_ones(app.gp_.challenge_flags_.get()) > 2;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[maestro_2].reward_), awarded);
     }},

    {SystemString::achievement_triage_name,
     SystemString::achievement_triage_description,
     "dynamite-ii",
     [](Platform&, App& app) {
         // invoked manually through achievements::raise().
         return false;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[triage].reward_), awarded);
     }},

    {SystemString::achievement_banana_man_name,
     SystemString::achievement_banana_man_description,
     "banana-plant",
     [](Platform&, App& app) {
         // invoked manually through achievements::raise().
         return false;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[banana_man].reward_), awarded);
     }},

    {SystemString::achievement_ancient_weapon_name,
     SystemString::achievement_ancient_weapon_description,
     "decimator",
     [](Platform&, App& app) {
         // invoked manually through achievements::raise().
         return false;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[ancient_weapon].reward_), awarded);
     }},

    {SystemString::achievement_ship_of_theseus_name,
     SystemString::achievement_ship_of_theseus_description,
     "mirror-hull",
     [](Platform& pfrm, App& app) {
         if (not app.opponent_island()) {
             return false;
         }

         auto& p = app.player_island();
         auto& o = *app.opponent_island();

         if (p.rooms().size() < 20) {
             return false;
         }

         // At all costs, try to avoid a strong comparison between the two
         // islands.
         if (p.rooms().size() not_eq o.rooms().size() or
             p.power_supply() not_eq o.power_supply() or
             p.power_drain() not_eq o.power_drain() or
             p.core_count() not_eq o.core_count() or
             p.workshop_count() not_eq o.workshop_count()) {

             return false;
         }

         for (auto& room : o.rooms()) {
             auto x = room->position().x;
             // Flip over the x-axis.
             x = p.terrain().size() - 1 - x;

             if (auto other = p.get_room({x, room->position().y})) {
                 if (not str_eq(other->name(), room->name())) {
                     return false;
                 }
             } else {
                 return false;
             }
         }

         return true;
     },
     [](Platform& pfrm, App&, bool awarded) {
         set_enabled(metaclass_index(info[ship_of_theseus].reward_), awarded);
     }},

    {SystemString::achievement_lemons_name,
     SystemString::achievement_lemons_description,
     "lemon-tree",
     [](Platform&, App& app) {
         // invoked manually through achievements::raise().
         return false;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[lemons].reward_), awarded);
     }},

    {SystemString::achievement_new_colossus_name,
     SystemString::achievement_new_colossus_description,
     "lady-liberty",
     [](Platform&, App& app) {
         if (player_island(app).character_count() >= 7) {
             u8 count = 0;
             for (auto& room : player_island(app).rooms()) {
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
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[new_colossus].reward_), awarded);
     }},

    {SystemString::achievement_meltdown_name,
     SystemString::achievement_meltdown_description,
     "radiator",
     [](Platform&, App& app) {
         // invoked manually through achievements::raise().
         return false;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[meltdown].reward_), awarded);
     }},

    {SystemString::achievement_completionist_name,
     SystemString::achievement_completionist_description,
     "gold",
     [](Platform&, App& app) {
         auto bc = count_ones(app.gp_.challenge_flags_.get());
         if (bc < 6) {
             // FIXME: use something other than hard-coding four!
             return false;
         }

         return true;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[completionist].reward_), awarded);
     }},

    {SystemString::achievement_mycelium_name,
     SystemString::achievement_mycelium_description,
     "mycelium",
     [](Platform&, App& app) {
         // invoked manually through achievements::raise().
         return false;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[mycelium].reward_), awarded);
     }},

    {SystemString::achievement_primitive_name,
     SystemString::achievement_primitive_description,
     "sticky-piston",
     [](Platform&, App& app) {
         return app.zone() > 3 and
                not(app.persistent_data().state_flags_.get() &
                    PersistentData::workshop_built);
     },
     [](Platform&, App& app, bool awarded) {
         set_enabled(metaclass_index(info[primitive].reward_), awarded);
     }}};



void init(Platform& pfrm, App& app)
{
    auto flags = app.gp_.achievement_flags_;

    for (int i = 0; i < Achievement::count; ++i) {
        const u64 flag = 1 << i;

        if (flags.get() & flag) {
            info[i].award_(pfrm, app, true);
        }
    }
}



static Bitvector<Achievement::count> raised_achievements;



void raise(Platform& pfrm, App& app, Achievement achievement)
{
    auto& flags = app.gp_.achievement_flags_;
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



Achievement update(Platform& pfrm, App& app)
{
    auto check_achievement = last_achievement + 1;
    if (check_achievement == Achievement::count) {
        check_achievement = (Achievement)((int)Achievement::none + 1);
    }

    last_achievement = (Achievement)check_achievement;

    auto& flags = app.gp_.achievement_flags_;

    const u64 flag = 1 << check_achievement;

    static_assert(Achievement::count < (sizeof(flag) * 8),
                  "More than 64 achievements, i.e. achievement bit does not "
                  "fit in a u64.o");

    if (not(flags.get() & flag)) {
        if (raised_achievements.get(check_achievement) or
            info[check_achievement].match_(pfrm, app)) {
            raised_achievements.set(check_achievement, false);
            flags.set(flags.get() | flag);
            save::store_global_data(pfrm, app.gp_);
            return static_cast<Achievement>(check_achievement);
        }
    }

    return Achievement::none;
}



void lock(Platform& pfrm, App& app, Achievement achievement)
{
    auto& flags = app.gp_.achievement_flags_;
    const u64 flag = 1 << achievement;

    flags.set(flags.get() & ~flag);

    save::store_global_data(pfrm, app.gp_);

    info[achievement].award_(pfrm, app, false);
}



bool unlock(Platform& pfrm, App& app, Achievement achievement)
{
    auto& flags = app.gp_.achievement_flags_;
    const u64 flag = 1 << achievement;

    if (not(flags.get() & flag)) {
        flags.set(flags.get() | flag);
        save::store_global_data(pfrm, app.gp_);
        return true;
    }

    return false;
}



void award(Platform& pfrm, App& app, Achievement achievement)
{
    info[achievement].award_(pfrm, app, true);

    time_stream::event::Achievement e;
    e.which_ = achievement;
    app.time_stream().push(app.level_timer(), e);
}



bool is_unlocked(App& app, Achievement achievement)
{
    auto& flags = app.gp_.achievement_flags_;
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
