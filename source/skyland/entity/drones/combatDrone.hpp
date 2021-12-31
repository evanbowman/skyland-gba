#pragma once

#include "drone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"



namespace skyland {



class CombatDrone : public Drone {
public:
    CombatDrone(Island* parent, Island* destination, const Vec2<u8>& grid_pos)
        : Drone(parent, destination, grid_pos)
    {
    }


    static const char* get_name()
    {
        return "combat-drone";
    }


    const char* name() const override
    {
        return get_name();
    }


    static u16 icon()
    {
        // TODO...
        return 512;
    }


    static u16 unsel_icon()
    {
        // TODO...
        return 512;
    }
};



} // namespace skyland
