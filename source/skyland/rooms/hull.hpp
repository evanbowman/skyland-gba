#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



class Hull : public Room {
public:
    Hull(Island* parent, const Vec2<u8>& position, const char* n = name());


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    static u32 properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 1.f;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "hull";
    }


    static SystemString ui_name()
    {
        return SystemString::block_hull;
    }


    static Icon icon()
    {
        return 520;
    }


    static Icon unsel_icon()
    {
        return 504;
    }
};



} // namespace skyland
