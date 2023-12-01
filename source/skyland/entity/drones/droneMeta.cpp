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


#include "droneMeta.hpp"
#include "attackDrone.hpp"
#include "combatDrone.hpp"
#include "flakDrone.hpp"
#include "repairDrone.hpp"



namespace skyland
{



static const DroneMetatable<AttackDrone, CombatDrone, RepairDrone, FlakDrone>
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
