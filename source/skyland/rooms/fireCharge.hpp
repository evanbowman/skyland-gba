#pragma once

#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland
{



extern SharedVariable fire_charge_reload_ms;



class FireCharge : public Weapon
{
public:
    FireCharge(Island* parent, const Vec2<u8>& position);


    void fire(Platform& pfrm, App& app) override;
    Microseconds reload() const override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


    static u32 properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::manufactory_required |
               RoomProperties::multiplayer_unsupported;
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
        return "fire-charge";
    }


    static SystemString ui_name()
    {
        return SystemString::block_fire_charge;
    }


    static Float ai_base_weight()
    {
        return 900.f;
    }


    static Icon icon()
    {
        return 2312;
    }


    static Icon unsel_icon()
    {
        return 2328;
    }
};


} // namespace skyland