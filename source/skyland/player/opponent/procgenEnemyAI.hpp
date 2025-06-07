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


#include "enemyAI.hpp"
#include "number/random.hpp"
#include "skyland/island.hpp"



namespace skyland
{



class ProcgenEnemyAI : public EnemyAI
{
public:
    ProcgenEnemyAI(rng::LinearGenerator seed, u8 difficulty);


    void update(Time delta) override;


    void generate_level();


    void set_levelgen_count(int count);


private:
    void generate_power_sources();
    void generate_stairwells();
    void generate_secondary_rooms();
    void generate_foundation();
    void generate_hull();
    void generate_weapons(int max);
    void generate_forcefields();
    void generate_characters();
    void generate_decorations();
    void generate_radiators();
    void generate_walls_behind_weapons();
    void generate_missile_defenses();

    void cleanup_unused_terrain();


    bool has_space(const RoomCoord& loc, const RoomCoord& sz);


    Power power_remaining() const;


    void place_room_random_loc(int x_start, const char* room_name);


    void place_room_adjacent(const char* room_name);


    rng::LinearGenerator rng_source_;
    u8 difficulty_ = 1;

    Vec2<u8> levelgen_size_;
    int levelgen_enemy_count_ = 0;

    int core_count_ = 0;

    Optional<Text> level_text_;
};



} // namespace skyland
