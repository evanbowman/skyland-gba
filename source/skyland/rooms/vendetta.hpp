#pragma once


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "weapon.hpp"



namespace skyland {



extern SharedVariable vendetta_reload_ms;



class Vendetta : public Weapon {
public:
    Vendetta(Island* parent, const Vec2<u8>& position);


    void fire(Platform& pfrm, App& app) override;
    Microseconds reload() const override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    bool has_roof() override
    {
        return false;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static const char* name()
    {
        return "vendetta";
    }


    static Float ai_base_weight()
    {
        return 12.f;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 552;
    }


    static Icon unsel_icon()
    {
        return 536;
    }


    static Conditions::Value conditions()
    {
        return Conditions::foundry_required;
    }

};


} // namespace skyland
