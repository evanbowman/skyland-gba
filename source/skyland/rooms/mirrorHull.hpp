#pragma once

#include "hull.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class MirrorHull : public Hull
{
public:
    MirrorHull(Island* parent, const Vec2<u8>& position);


    static const char* name()
    {
        return "mirror-hull";
    }


    static SystemString ui_name()
    {
        return SystemString::block_mirror_hull;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_mirror_hull)->c_str();
    }


    static u32 properties()
    {
        return RoomProperties::manufactory_required |
            Hull::properties() | RoomProperties::disabled_in_tutorials//  |
               // RoomProperties::locked_by_default
            ;
    }


    static Icon icon()
    {
        return 1624;
    }


    static Icon unsel_icon()
    {
        return 1640;
    }


    static Float ai_base_weight()
    {
        return 0.5f;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;


    void render_exterior(App& app, u8 buffer[16][16]) override;
};



} // namespace skyland
