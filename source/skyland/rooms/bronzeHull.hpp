#pragma once

#include "hull.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class BronzeHull : public Hull {
public:
    BronzeHull(Island* parent, const Vec2<u8>& position);


    using Hull::Hull;


    static const char* name()
    {
        return "bronze-hull";
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += "";
    }


    static u32 properties()
    {
        return Hull::properties() | RoomProperties::disabled_in_tutorials |
               RoomProperties::locked_by_default;
    }


    static Icon icon()
    {
        return 1624;
    }


    static Icon unsel_icon()
    {
        return 1640;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;
};



} // namespace skyland
