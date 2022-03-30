#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Barrier : public Room
{
public:
    Barrier(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void apply_damage(Platform&, App&, Health damage) override;


    static const char* name()
    {
        return "barrier";
    }


    static SystemString ui_name()
    {
        return SystemString::block_barrier;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_barrier)->c_str();
    }


    static Float ai_base_weight()
    {
        return 0.00001f;
    }


    bool description_visible() override
    {
        return true;
    }


    static Category category()
    {
        return Category::wall;
    }


    static u32 properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount | RoomProperties::sandbox_mode_only;
    }


    static Icon icon()
    {
        return 1624;
    }


    static Icon unsel_icon()
    {
        return 1640;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;
};



} // namespace skyland
