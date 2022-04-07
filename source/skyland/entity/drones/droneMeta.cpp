////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "droneMeta.hpp"
#include "attackDrone.hpp"
#include "combatDrone.hpp"
#include "flakDrone.hpp"
#include "repairDrone.hpp"



namespace skyland
{



static DroneMetatable<10, AttackDrone, CombatDrone, RepairDrone, FlakDrone>
    __drone_metatable;



std::pair<DroneMeta*, int> drone_metatable()
{
    return {__drone_metatable.table_, __drone_metatable.size()};
}



DroneMeta* DroneMeta::load(const char* name)
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
