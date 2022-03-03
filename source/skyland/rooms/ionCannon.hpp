#pragma once


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland {



extern SharedVariable ion_cannon_reload_ms;



class IonCannon : public Weapon
{
public:
    IonCannon(Island* parent, const Vec2<u8>& position);


    void fire(Platform& pfrm, App& app) override;
    Microseconds reload() const override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "ion-cannon";
    }


    static SystemString ui_name()
    {
        return SystemString::block_ion_cannon;
    }



    static Float ai_base_weight()
    {
        return 800.f;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static u32 properties()
    {
        return RoomProperties::workshop_required | RoomProperties::roof_hidden;
    }


    static Icon icon()
    {
        return 840;
    }


    static Icon unsel_icon()
    {
        return 824;
    }
};


} // namespace skyland
