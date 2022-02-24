#include "achievement.hpp"
#include "room_metatable.hpp"
#include "save.hpp"
#include "skyland.hpp"
#include "timeStreamEvent.hpp"



namespace skyland::achievements {



struct AchievementInfo {
    const char* name_;
    const char* description_;
    const char* reward_;

    bool (*match_)(Platform&, App&);
    void (*award_)(Platform&, App&, bool);
};



static const AchievementInfo info[Achievement::count] = {
    {"none",
     "none",
     "none",
     [](Platform&, App&) { return false; },
     [](Platform&, App&, bool) {}},

    {"Builder",
     "Build an island with more than ten structures!",
     "masonry",
     [](Platform&, App& app) {
         return app.player_island().rooms().size() > 10;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[builder].reward_), awarded);
     }},

    {"Architect",
     "Build an island with more than twenty structures!",
     "bridge",
     [](Platform&, App& app) {
         return app.player_island().rooms().size() > 20;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[architect].reward_), awarded);
     }},

    {"Explorer",
     "Reach zone 2!",
     "coconut-palm",
     [](Platform&, App& app) { return app.zone() > 1; },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[explorer].reward_), awarded);
     }},

    {"Strategist",
     "Reach zone 3!",
     "statue",
     [](Platform&, App& app) { return app.zone() > 2; },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[strategist].reward_), awarded);
     }},

    {"Stronghold",
     "Build a castle with at least four power cores!",
     "bronze-hull",
     [](Platform&, App& app) { return app.player_island().core_count() > 3; },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[stronghold].reward_), awarded);
     }},

    {"dynamite",
     "Complete any challenge level!",
     "dynamite",
     [](Platform&, App& app) { return app.gp_.challenge_flags_ not_eq 0; },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[maestro_1].reward_), awarded);
     }},

    {"Virtuoso (1)",
     "Complete three challenge levels!",
     "speaker",
     [](Platform&, App& app) {
         u64 v = app.gp_.challenge_flags_;
         u32 upper;
         u32 lower;
         memcpy(&upper, &v, sizeof upper);
         memcpy(&lower, (u8*)&v + 4, sizeof lower);

         auto bc = count_1bits(upper) + count_1bits(lower);
         return bc > 2;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[maestro_1].reward_), awarded);
     }},

    {"Virtuoso (2)",
     "Complete three challenge levels!",
     "synth",
     [](Platform&, App& app) {
         u64 v = app.gp_.challenge_flags_;
         u32 upper;
         u32 lower;
         memcpy(&upper, &v, sizeof upper);
         memcpy(&lower, (u8*)&v + 4, sizeof lower);

         auto bc = count_1bits(upper) + count_1bits(lower);
         return bc > 2;
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[maestro_2].reward_), awarded);
     }},

    {"Triage",
     "Destroy invading goblins with dynamite!",
     "dynamite-ii",
     [](Platform&, App& app) {
         return is_enabled(metaclass_index(info[triage].reward_));
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[triage].reward_), awarded);
     }},

    {"Borrowed tech",
     "Destroy a decimator by plundering!",
     "decimator",
     [](Platform&, App& app) {
         // Yeah, this is a pretty bad hack. When the PlayerP1 class receives a
         // notification that the player plundered a room, it immediately
         // enables the decimator item if the player plundered a
         // decimator. Then, this code sees the signal and raises an alert. Sort
         // of a backwards way of doing things. We could unlock() the
         // achievement from the player class, but then the player would be
         // responsible for creating a notification scene, which just isn't
         // realistic, because the player isn't supposed to have control over
         // the scene transition logic.
         // Please excuse this spaghetti code.
         return is_enabled(metaclass_index(info[ancient_weapon].reward_));
     },
     [](Platform&, App&, bool awarded) {
         set_enabled(metaclass_index(info[ancient_weapon].reward_), awarded);
     }}};



void init(Platform& pfrm, App& app)
{
    auto flags = app.gp_.achievement_flags_;

    for (int i = 0; i < Achievement::count; ++i) {
        const u64 flag = 1 << i;

        if (flags & flag) {
            info[i].award_(pfrm, app, true);
        }
    }
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

    if (not(flags & flag)) {
        if (info[check_achievement].match_(pfrm, app)) {
            flags |= flag;
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

    flags &= ~flag;

    save::store_global_data(pfrm, app.gp_);

    info[achievement].award_(pfrm, app, false);
}



bool unlock(Platform& pfrm, App& app, Achievement achievement)
{
    auto& flags = app.gp_.achievement_flags_;
    const u64 flag = 1 << achievement;

    if (not(flags & flag)) {
        flags |= flag;
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
    app.time_stream().push(pfrm, app.level_timer(), e);
}



bool is_unlocked(App& app, Achievement achievement)
{
    auto& flags = app.gp_.achievement_flags_;
    const u64 flag = 1 << achievement;

    return flags & flag;
}



const char* description(Achievement achievement)
{
    return info[achievement].description_;
}



const char* name(Achievement achievement)
{
    return info[achievement].name_;
}



const char* reward(Achievement achievement)
{
    return info[achievement].reward_;
}



} // namespace skyland::achievements
