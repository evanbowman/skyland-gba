#pragma once


#include "enemyAI.hpp"
#include "skyland/island.hpp"



namespace skyland {



class ProcgenEnemyAI : public EnemyAI {
public:
    ProcgenEnemyAI(u8 difficulty);


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void generate_level(Platform& pfrm, App& app);


    void set_levelgen_count(int count);


private:

    void generate_power_sources(Platform& pfrm, App& app);
    void generate_stairwells(Platform& pfrm, App& app);
    void generate_secondary_rooms(Platform& pfrm, App& app);
    void generate_foundation(Platform& pfrm, App& app);
    void generate_hull(Platform& pfrm, App& app);
    void generate_rear_hull(Platform& pfrm, App& app); // TODO!
    void generate_weapons(Platform& pfrm, App& app, int max);
    void generate_forcefields(Platform& pfrm, App& app);
    void generate_characters(Platform& pfrm, App& app);
    void generate_decorations(Platform& pfrm, App& app);
    void generate_radiators(Platform& pfrm, App& app);

    void cleanup_unused_terrain(Platform& pfrm, App& app);


    bool has_space(App& app, const Vec2<u8>& loc, const Vec2<u8>& sz);


    Power power_remaining(App& app) const;


    void place_room_random_loc(Platform& pfrm,
                               App& app,
                               int x_start,
                               const char* room_name);


    void place_room_adjacent(Platform& pfrm,
                             App& app,
                             const char* room_name);


    u8 difficulty_ = 1;

    Vec2<u8> levelgen_size_;
    int levelgen_enemy_count_ = 0;

    int core_count_ = 0;

    std::optional<Text> level_text_;
};



} // namespace skyland
