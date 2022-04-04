#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class SolarCell : public Room
{
public:
    SolarCell(Island* parent, const Vec2<u8>& position) :
        Room(parent, name(), position)
    {
    }


    Power power_usage() const override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::power;
    }


    static Float ai_base_weight()
    {
        return 1000.f;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static const char* name()
    {
        return "solar-cell";
    }


    static SystemString ui_name()
    {
        return SystemString::block_solar_cell;
    }


    static Icon icon()
    {
        return 2280;
    }


    static Icon unsel_icon()
    {
        return 2296;
    }


    static u32 properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::roof_hidden |
               RoomProperties::disallow_chimney;
    }
};



} // namespace skyland
