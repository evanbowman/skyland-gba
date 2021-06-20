#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Forcefield : public Room {
public:
    Forcefield(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    void plot_walkable_zones(bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }



    bool has_roof() override
    {
        return false;
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


    static Coins cost()
    {
        return 300;
    }


    static Power consumes_power()
    {
        return 40;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }
};



} // namespace skyland
