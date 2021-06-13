#pragma once

#include "skyland/room.hpp"
#include "skyland/coins.hpp"



namespace skyland {



class Core : public Room {
public:
    Core(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "power core";
    }


    static Coins cost()
    {
        return 1500;
    }
};



} // namespace skyland
