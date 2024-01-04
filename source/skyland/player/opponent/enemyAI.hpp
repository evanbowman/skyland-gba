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


#include "allocator.hpp"
#include "opponent.hpp"
#include "skyland/coins.hpp"
#include "skyland/island.hpp"



namespace skyland
{



class Drone;
class Cannon;
class BeamGun;
class FlakGun;
class DroneBay;
class ClumpBomb;
class IonCannon;
class FireCharge;
class RocketSilo;
class MissileSilo;
class BasicCharacter;



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


    static void assign_boarded_character(BasicCharacter& character,
                                         Player* owner,
                                         Island* ai_island,
                                         Island* target_island);


    static void assign_local_character(BasicCharacter& character,
                                       Player* owner,
                                       Island* ai_island,
                                       Island* target_island,
                                       bool repair_priority = false);


    static void update_room(Room& room,
                            const Bitmatrix<16, 16>& matrix,
                            Player* owner,
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


    void combat_drone_set_target(const Bitmatrix<16, 16>& matrix, Drone& drone);


    void offensive_drone_set_target(const Bitmatrix<16, 16>& matrix,
                                    Drone& drone);


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
