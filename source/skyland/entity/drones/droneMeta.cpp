////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "droneMeta.hpp"
#include "attackDrone.hpp"
#include "combatDrone.hpp"
#include "flakDrone.hpp"
#include "reconstructionDrone.hpp"
#include "repairDrone.hpp"



namespace skyland
{



static const DroneMetatable<AttackDrone,
                            CombatDrone,
                            RepairDrone,
                            ReconstructionDrone,
                            FlakDrone>
    __drone_metatable;



std::pair<const DroneMeta*, int> drone_metatable()
{
    return {__drone_metatable.table_, __drone_metatable.size()};
}



const DroneMeta* DroneMeta::load(const char* name)
{
    auto [dt, ds] = drone_metatable();

    for (int i = 0; i < ds; ++i) {
        if (str_cmp(dt[i]->name(), name) == 0) {
            return &dt[i];
        }
    }

    return nullptr;
}



int DroneMeta::index(const char* name)
{
    auto [dt, ds] = drone_metatable();
    for (u8 i = 0; i < ds; ++i) {
        if (str_cmp(dt[i]->name(), name) == 0) {
            return i;
        }
    }
    return 0;
}



} // namespace skyland
