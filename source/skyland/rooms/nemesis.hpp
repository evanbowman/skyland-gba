#pragma once


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "weapon.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



extern SharedVariable nemesis_reload_ms;



class Nemesis : public Weapon {
public:
    Nemesis(Island* parent, const Vec2<u8>& position);


    void fire(Platform& pfrm, App& app) override;
    Microseconds reload() const override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static u32 properties()
    {
        return Weapon::properties() | RoomProperties::roof_hidden |
               RoomProperties::manufactory_required;
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
        return "nemesis";
    }


    static SystemString ui_name()
    {
        return SystemString::block_nemesis;
    }


    static Float ai_base_weight()
    {
        return 12.f;
    }


    static Category category()
    {
        return Category::weapon;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 1592;
    }


    static Icon unsel_icon()
    {
        return 1608;
    }
};



} // namespace skyland
