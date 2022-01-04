#include "droneMeta.hpp"
#include "attackDrone.hpp"
#include "combatDrone.hpp"
#include "flakDrone.hpp"
#include "repairDrone.hpp"



namespace skyland {



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
