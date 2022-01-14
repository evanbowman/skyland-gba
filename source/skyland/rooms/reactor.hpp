#pragma once

#include "skyland/room.hpp"



namespace skyland {



class Reactor : public Room {
public:

    Reactor(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    bool has_chimney() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 999.f;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "reactor";
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
        return Conditions::foundry_required;
    }
};



}
