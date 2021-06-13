#pragma once

#include "skyland/room.hpp"



namespace skyland {



class ExteriorWall : public Room {
public:
    ExteriorWall(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;
};




}
