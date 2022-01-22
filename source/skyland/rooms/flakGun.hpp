#pragma once


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "weapon.hpp"



namespace skyland {



extern SharedVariable flak_gun_reload_ms;



class FlakGun : public Weapon {
public:
    FlakGun(Island* parent, const Vec2<u8>& position);


    void fire(Platform& pfrm, App& app) override;
    Microseconds reload() const override;


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


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
        return "flak-gun";
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // characters cannot walk through a flak gun.
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


    static Icon icon()
    {
        return 936;
    }


    static Icon unsel_icon()
    {
        return 920;
    }
};



} // namespace skyland
