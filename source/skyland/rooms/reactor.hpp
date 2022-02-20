#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



class Reactor : public Room {
public:
    Reactor(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static Float ai_base_weight()
    {
        return 1200.f;
    }


    static Vec2<u8> size()
    {
        return {2, 3};
    }


    static const char* name()
    {
        return "reactor";
    }


    static SystemString ui_name()
    {
        return SystemString::block_reactor;
    }


    static Icon icon()
    {
        return 744;
    }


    static Icon unsel_icon()
    {
        return 728;
    }


    static u32 properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::has_chimney | RoomProperties::accepts_ion_damage;
    }
};



} // namespace skyland
