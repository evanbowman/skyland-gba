#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Stairwell : public Room {
public:
    Stairwell(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    static void format_description(StringBuffer<512>& buffer);


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override;


    static Float ai_base_weight()
    {
        return 50.f;
    }


    static Vec2<u8> size()
    {
        return {1, 4};
    }


    static const char* name()
    {
        return "stairwell";
    }


    static Icon icon()
    {
        return 616;
    }


    static Icon unsel_icon()
    {
        return 600;
    }
};



} // namespace skyland
