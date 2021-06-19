#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class ExteriorWall : public Room {
public:
    ExteriorWall(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "retaining wall";
    }


    static Coins cost()
    {
        return 300;
    }

};



} // namespace skyland
