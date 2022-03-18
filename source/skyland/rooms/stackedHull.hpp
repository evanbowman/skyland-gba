#pragma once

#include "hull.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class StackedHull : public Hull
{
public:
    StackedHull(Island* parent, const Vec2<u8>& position);


    using Hull::Hull;


    static const char* name()
    {
        return "stacked-hull";
    }


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
    }


    static SystemString ui_name()
    {
        return SystemString::block_stacked_hull;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_stacked_hull)->c_str();
    }


    static u32 properties()
    {
        return Hull::properties() | RoomProperties::disabled_in_tutorials |
               RoomProperties::manufactory_required;
    }


    static Icon icon()
    {
        return 2024;
    }


    static Icon unsel_icon()
    {
        return 2040;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;
};



} // namespace skyland
