#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class PlunderedRoom : public Room {
public:
    PlunderedRoom(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


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


    static RoomProperties::Value properties()
    {
        return RoomProperties::not_constructible;
    }
};



} // namespace skyland
