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


#include "allocator.hpp"
#include "opponent.hpp"
#include "skyland/coins.hpp"
#include "skyland/island.hpp"



namespace skyland
{



class Drone;
class Cannon;
class ArcGun;
class BeamGun;
class FlakGun;
class Ballista;
class DroneBay;
class ClumpBomb;
class IonCannon;
class FireCharge;
class RocketSilo;
class MissileSilo;
class Character;



class EnemyAI : public Opponent
{
public:
    EnemyAI()
    {
        ai_island_ = nullptr;
        target_island_ = nullptr;
    }


    EnemyAI(Island* ai_island, Island* target_island)
    {
        ai_island_ = ai_island;
        target_island_ = target_island;
    }


    void update(Time delta) override;


    void on_room_damaged(Room&) override;


    static void assign_boarded_character(Character& character,
                                         Player* owner,
                                         Island* ai_island,
                                         Island* target_island);


    static void assign_local_character(Character& character,
                                       Player* owner,
                                       Island* ai_island,
                                       bool repair_priority = false);


    static void update_room(Room& room,
                            const Bitmatrix<16, 16>& matrix,
                            Player* owner,
                            Island* ai_island,
                            Island* target_island);


    static void drone_set_target(const Bitmatrix<16, 16>& matrix,
                                 Drone& drone,
                                 Island* ai_island,
                                 Island* target_island);


private:
    void update_drone_ai();
    void update_local_chr_ai();
    void update_boarded_chr_ai();
    void update_weapon_ai();


    static void set_target(const Bitmatrix<16, 16>& matrix,
                           Room& generic_gun,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(const Bitmatrix<16, 16>& matrix,
                           ArcGun& generic_gun,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(const Bitmatrix<16, 16>& matrix,
                           FireCharge& fire_charge,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(const Bitmatrix<16, 16>& matrix,
                           MissileSilo& silo,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target_rocketsilo(const Bitmatrix<16, 16>& matrix,
                                      Room& silo,
                                      Player* owner,
                                      Island* ai_island,
                                      Island* target_island);

    static void set_target(const Bitmatrix<16, 16>& matrix,
                           ClumpBomb& silo,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(const Bitmatrix<16, 16>& matrix,
                           IonCannon& cannon,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);


    static void set_target(const Bitmatrix<16, 16>& matrix,
                           BeamGun& gun,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);


    static void set_target(const Bitmatrix<16, 16>& matrix,
                           FlakGun& gun,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);


    static void set_target(const Bitmatrix<16, 16>& matrix,
                           Ballista& ballista,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);


    static void combat_drone_set_target(const Bitmatrix<16, 16>& matrix,
                                        Drone& drone,
                                        Island* ai_island,
                                        Island* target_island);


    static void offensive_drone_set_target(const Bitmatrix<16, 16>& matrix,
                                           Drone& drone,
                                           Island* ai_island,
                                           Island* target_island);


    static void update_drone_bay(const Bitmatrix<16, 16>& matrix,
                                 DroneBay& db,
                                 Island* ai_island,
                                 Island* target_island);


    void resolve_insufficient_power();


    static void assign_weapon_target(Room& weapon,
                                     const RoomCoord& target,
                                     Island* ai_island);


    Island* ai_island_;
    Island* target_island_;


    static const auto next_action_timeout = seconds(1);
    static const auto insufficent_power_resolve_timeout = (milliseconds(100));
    static const auto character_reassign_timeout = milliseconds(6000);
    static const auto drone_update_timeout_ = seconds(1);


    Time next_action_timer_ = next_action_timeout;

    Time character_reassign_timer_ =
        character_reassign_timeout + milliseconds(500);
    Time local_character_reassign_timer_ = character_reassign_timeout;


    Time insufficent_power_resolve_timer_ = 0;

    Time drone_update_timer_ = 0;

    u32 room_update_index_ = 0;
};



} // namespace skyland
