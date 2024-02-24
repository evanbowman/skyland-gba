////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
