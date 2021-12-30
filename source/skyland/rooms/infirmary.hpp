#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Infirmary : public Room {
public:
    Infirmary(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "infirmary";
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
