#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Radar : public Room {
public:
    Radar(Island* parent, const Vec2<u8>& position);


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "radar";
    }


    static Coins cost()
    {
        return 1000;
    }


    static Float ai_base_weight()
    {
        return 400.f;
    }


    static Power consumes_power()
    {
        return 60;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


    void plot_walkable_zones(bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    bool has_roof() override
    {
        return false;
    }
};



} // namespace skyland
