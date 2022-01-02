#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class PlunderedRoom : public Room {
public:
    PlunderedRoom(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(u8 buffer[16][16]) override;
    void render_exterior(u8 buffer[16][16]) override;


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 10.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "plundered-room";
    }


    static Conditions::Value conditions()
    {
        return Conditions::not_constructible;
    }
};



} // namespace skyland
