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


#include "enemyAI.hpp"
#include "number/random.hpp"
#include "skyland/island.hpp"



namespace skyland
{



class ProcgenEnemyAI : public EnemyAI
{
public:
    ProcgenEnemyAI(rng::LinearGenerator seed, u8 difficulty);


    void update(Microseconds delta) override;


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

    std::optional<Text> level_text_;
};



} // namespace skyland
