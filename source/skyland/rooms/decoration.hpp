#pragma once

#include "skyland/room.hpp"



namespace skyland {



class Decoration : public Room
{
public:
    using Room::Room;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
    }


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
    }


    static u32 properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::locked_by_default | RoomProperties::fragile;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 1.f;
    }


    static Category category()
    {
        return Category::decoration;
    }
};



} // namespace skyland
