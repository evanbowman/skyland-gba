#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Workshop : public Room {
public:
    Workshop(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "workshop";
    }


    static Float ai_base_weight()
    {
        return 700.f;
    }


    static Coins cost()
    {
        return 2000;
    }
};



} // namespace skyland
