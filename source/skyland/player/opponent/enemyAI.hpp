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


    void update(Platform&, App&, Microseconds delta) override;


    void on_room_damaged(Platform&, App& app, Room&) override;


    void add_coins(Coins value)
    {
        coins_ += value;
    }


    static void assign_boarded_character(Platform&,
                                         App&,
                                         BasicCharacter& character,
                                         Player* owner,
                                         Island* ai_island,
                                         Island* target_island);


    static void assign_local_character(Platform&,
                                       App&,
                                       BasicCharacter& character,
                                       Player* owner,
                                       Island* ai_island,
                                       Island* target_island,
                                       bool repair_priority = false);


    static void update_room(Platform&,
                            App&,
                            Room& room,
                            const Bitmatrix<16, 16>& matrix,
                            Player* owner,
                            Island* ai_island,
                            Island* target_island);

private:
    static void set_target(Platform&,
                           App&,
                           const Bitmatrix<16, 16>& matrix,
                           Room& generic_gun,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(Platform&,
                           App&,
                           const Bitmatrix<16, 16>& matrix,
                           FireCharge& fire_charge,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(Platform&,
                           App&,
                           const Bitmatrix<16, 16>& matrix,
                           MissileSilo& silo,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(Platform&,
                           App&,
                           const Bitmatrix<16, 16>& matrix,
                           RocketSilo& silo,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);

    static void set_target(Platform&,
                           App&,
                           const Bitmatrix<16, 16>& matrix,
                           IonCannon& cannon,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);


    static void set_target(Platform&,
                           App&,
                           const Bitmatrix<16, 16>& matrix,
                           BeamGun& gun,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);


    static void set_target(Platform&,
                           App&,
                           const Bitmatrix<16, 16>& matrix,
                           FlakGun& gun,
                           Player* owner,
                           Island* ai_island,
                           Island* target_island);


    void combat_drone_set_target(Platform&,
                                 App&,
                                 const Bitmatrix<16, 16>& matrix,
                                 Drone& drone);


    void offensive_drone_set_target(Platform&,
                                    App&,
                                    const Bitmatrix<16, 16>& matrix,
                                    Drone& drone);


    static void update_drone_bay(Platform&,
                                 App&,
                                 const Bitmatrix<16, 16>& matrix,
                                 DroneBay& db,
                                 Island* ai_island,
                                 Island* target_island);


    void resolve_insufficient_power(Platform&, App&);


    static void assign_weapon_target(Platform&,
                                     App&,
                                     Room& weapon,
                                     const RoomCoord& target,
                                     Island* ai_island);


    Island* ai_island_;
    Island* target_island_;


    static const auto next_action_timeout = seconds(1);

    Microseconds next_action_timer_ = next_action_timeout;

    static const auto character_reassign_timeout = seconds(6);

    Microseconds character_reassign_timer_ = character_reassign_timeout;
    Microseconds local_character_reassign_timer_ = character_reassign_timeout;

    Coins coins_ = 0;

    static const auto insufficent_power_resolve_timeout = (milliseconds(100));

    Microseconds insufficent_power_resolve_timer_ = 0;

    static const auto drone_update_timeout_ = seconds(1);
    Microseconds drone_update_timer_ = 0;

    Microseconds total_time_ = 0;

    u32 room_update_index_ = 0;
};



} // namespace skyland
