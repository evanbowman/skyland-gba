#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Forcefield : public Room {
public:
    Forcefield(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    // Plenty of rooms, like missile silos, and forcefields, look super awkward
    // if the game spawns chimneys over top of them.
    bool disallow_chimney() override
    {
        return true;
    }


    bool has_roof() override
    {
        return false;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 10.f;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "forcefield";
    }


    static Icon icon()
    {
        return 712;
    }


    static Icon unsel_icon()
    {
        return 696;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }
};



} // namespace skyland
