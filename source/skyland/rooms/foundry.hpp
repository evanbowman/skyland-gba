#pragma once



#include "skyland/room.hpp"



namespace skyland {



class Foundry : public Room {
public:
    Foundry(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta);


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::factory;
    }


    static Vec2<u8> size()
    {
        return {3, 2};
    }


    static const char* name()
    {
        return "foundry";
    }


    static Icon icon()
    {
        return 776;
    }


    static Icon unsel_icon()
    {
        return 760;
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }
};



} // namespace skyland
