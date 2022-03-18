#pragma once


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland
{



extern SharedVariable missile_silo_reload_ms;



class MissileSilo : public Weapon
{
public:
    MissileSilo(Island* parent, const Vec2<u8>& position);


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
        return {1, 2};
    }


    static const char* name()
    {
        return "missile-silo";
    }


    static SystemString ui_name()
    {
        return SystemString::block_missile_silo;
    }


    static Float ai_base_weight()
    {
        return 600.f;
    }


    static Icon icon()
    {
        return 584;
    }


    static Icon unsel_icon()
    {
        return 568;
    }


    static u32 properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }
};


} // namespace skyland
