#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



class Infirmary : public Room
{
public:
    Infirmary(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "infirmary";
    }


    static SystemString ui_name()
    {
        return SystemString::block_infirmary;
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    static Icon icon()
    {
        return 680;
    }


    static Icon unsel_icon()
    {
        return 664;
    }


private:
    Microseconds heal_timer_ = 0;
};



} // namespace skyland
