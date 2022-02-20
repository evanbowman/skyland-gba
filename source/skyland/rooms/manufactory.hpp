#pragma once



#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



class Manufactory : public Room {
public:
    Manufactory(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta);


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::factory;
    }


    static Vec2<u8> size()
    {
        return {3, 2};
    }


    static const char* name()
    {
        return "manufactory";
    }


    static SystemString ui_name()
    {
        return SystemString::block_manufactory;
    }


    static Icon icon()
    {
        return 776;
    }


    static Icon unsel_icon()
    {
        return 760;
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    static RoomProperties::Value properties()
    {
        return RoomProperties::workshop_required;
    }
};



} // namespace skyland
