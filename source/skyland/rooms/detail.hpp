#pragma once



#include "skyland/room.hpp"



namespace skyland {

class Detail : public Room
{
public:
    Detail(Island* parent, const Vec2<u8>& position, u16 tile) : Room(parent)
    {
    }


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;
};
} // namespace skyland
