#pragma once


#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



class Radiator : public Room
{
public:
    Radiator(Island* parent, const Vec2<u8>& position);


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    // static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    static u32 properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::workshop_required |
               RoomProperties::locked_by_default |
               RoomProperties::disabled_in_tutorials;
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
        return "radiator";
    }


    static SystemString ui_name()
    {
        return SystemString::block_radiator;
    }


    static Icon icon()
    {
        return 1832;
    }


    static Icon unsel_icon()
    {
        return 1848;
    }


private:
    void emit_radiation(Platform& pfrm, App& app);


    Microseconds damage_timer_ = 0;
};



} // namespace skyland
