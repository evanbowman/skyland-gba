////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "number/endian.hpp"



namespace skyland
{



struct CharacterStats
{
    using DamageRepairCount = u16;
    using EnemiesVanquished = u8;
    using FiresExtinguished = u8;
    using BattlesFought = u8;
    using StepCount = u16;
    using FavoriteRoom = u8;

    EnemiesVanquished enemies_vanquished_;
    BattlesFought battles_fought_;

    HostInteger<DamageRepairCount> damage_repaired_;
    HostInteger<StepCount> steps_taken_;

    FiresExtinguished fires_extinguished_;

    FavoriteRoom favorite_room_;


    template <typename T> static void inc(T& val)
    {
        if (val not_eq std::numeric_limits<T>::max()) {
            ++val;
        }
    }


    template <typename T> static void inc(HostInteger<T>& val)
    {
        if (val.get() not_eq std::numeric_limits<T>::max()) {
            val.set(val.get() + 1);
        }
    }


    template <typename T> static void dec(HostInteger<T>& val)
    {
        if (val.get() not_eq 0) {
            val.set(val.get() - 1);
        }
    }
};



struct CompleteCharacterStats
{
    CharacterStats info_;

    struct FavoriteRoomInfo
    {
        u8 metaclass_index_;
        HostInteger<u16> counter_;
    };

    FavoriteRoomInfo current_room_;
    FavoriteRoomInfo highest_room_[3];
};



} // Namespace skyland
