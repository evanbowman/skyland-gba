#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Mycelium : public Room
{
public:
    Mycelium(Island* parent,
             const Vec2<u8>& position,
             const char* name = name());


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static Category category()
    {
        return Category::wall;
    }


    static u32 properties()
    {
        return RoomProperties::disabled_in_tutorials |
               RoomProperties::manufactory_required |
               RoomProperties::roof_hidden | RoomProperties::salvage_disallowed;
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
        return "mycelium";
    }


    static SystemString ui_name()
    {
        return SystemString::block_mycelium;
    }


    bool description_visible() override
    {
        return true;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_mycelium)->c_str();
    }


    static Icon icon()
    {
        return 2184;
    }


    static Icon unsel_icon()
    {
        return 2200;
    }


    static constexpr const auto flood_time = seconds(8);


    Microseconds reload_time_remaining() const override
    {
        return flood_time - flood_timer_;
    }


protected:
    Microseconds flood_timer_ = 0;
};



} // namespace skyland
