#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Core : public Room {
public:
    Core(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    bool has_chimney() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 1000.f;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "power-core";
    }


    static Health full_health()
    {
        return 60;
    }


    static Coins cost()
    {
        return 3000;
    }


    static Power consumes_power()
    {
        return -150;
    }


    static Icon icon()
    {
        return 744;
    }


    static Icon unsel_icon()
    {
        return 728;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }
};



} // namespace skyland
