#pragma once


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland {



class Pummeler : public Weapon
{
public:
    Pummeler(Island* parent, const Vec2<u8>& position);


    void fire(Platform& pfrm, App& app) override;
    Microseconds reload() const override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static Category category()
    {
        return Category::weapon;
    }


    static u32 properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden;
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
        return "pummeler";
    }


    static SystemString ui_name()
    {
        return SystemString::block_cannon;
    }


    static Float ai_base_weight()
    {
        return 800.f;
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
};


} // namespace skyland
