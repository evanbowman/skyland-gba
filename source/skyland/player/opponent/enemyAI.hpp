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



namespace skyland
{



class Drone;
class Cannon;
class FlakGun;
class DroneBay;
class IonCannon;
class FireCharge;
class MissileSilo;
class BasicCharacter;



class EnemyAI : public Opponent
{
public:
    void update(Platform&, App&, Microseconds delta) override;


    void on_room_damaged(Platform&, App& app, Room&) override;


    void add_coins(Coins value)
    {
        coins_ += value;
    }


private:
    void
    update_room(Platform&, App&, Room& room, const Bitmatrix<16, 16>& matrix);

    void set_target(Platform&,
                    App&,
                    const Bitmatrix<16, 16>& matrix,
                    Room& generic_gun);

    void set_target(Platform&,
                    App&,
                    const Bitmatrix<16, 16>& matrix,
                    FireCharge& fire_charge);

    void set_target(Platform&,
                    App&,
                    const Bitmatrix<16, 16>& matrix,
                    MissileSilo& silo);

    void set_target(Platform&,
                    App&,
                    const Bitmatrix<16, 16>& matrix,
                    IonCannon& cannon);


    void
    set_target(Platform&, App&, const Bitmatrix<16, 16>& matrix, FlakGun& gun);


    void combat_drone_set_target(Platform&,
                                 App&,
                                 const Bitmatrix<16, 16>& matrix,
                                 Drone& drone);


    void offensive_drone_set_target(Platform&,
                                    App&,
                                    const Bitmatrix<16, 16>& matrix,
                                    Drone& drone);


    void update_drone_bay(Platform&,
                          App&,
                          const Bitmatrix<16, 16>& matrix,
                          DroneBay& db);


    void assign_boarded_character(Platform&, App&, BasicCharacter& character);


    void assign_local_character(Platform&, App&, BasicCharacter& character);


    void resolve_insufficient_power(Platform&, App&);


    void
    assign_weapon_target(Platform&, App&, Room& weapon, const RoomCoord& target);


    static const auto next_action_timeout = seconds(1);

    Microseconds next_action_timer_ = next_action_timeout;

    static const auto character_reassign_timeout = seconds(6);

    Microseconds character_reassign_timer_ = character_reassign_timeout;

    Coins coins_ = 0;

    static const auto insufficent_power_resolve_timeout = (milliseconds(100));

    Microseconds insufficent_power_resolve_timer_ = 0;

    static const auto drone_update_timeout_ = seconds(1);
    Microseconds drone_update_timer_ = 0;

    Microseconds score_subtract_timer_ = 0;
    Microseconds total_time_ = 0;

    u32 room_update_index_ = 0;
};



} // namespace skyland
