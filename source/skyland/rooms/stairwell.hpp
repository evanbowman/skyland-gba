#pragma once

#include "skyland/room.hpp"
#include "skyland/coins.hpp"



namespace skyland {



class Stairwell : public Room {
public:
    Stairwell(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    static Vec2<u8> size()
    {
        return {1, 4};
    }


    static const char* name()
    {
        return "stairwell";
    }


    static Coins cost()
    {
        return 700;
    }
};



} // namespace skyland
