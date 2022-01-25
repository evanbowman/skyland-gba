#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Radar : public Room {
public:
    Radar(Island* parent, const Vec2<u8>& position);



    static void format_description(StringBuffer<512>& buffer);



    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "radar";
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 400.f;
    }


    static Icon icon()
    {
        return 872;
    }


    static Icon unsel_icon()
    {
        return 856;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    bool has_roof() override
    {
        return false;
    }
};



} // namespace skyland
