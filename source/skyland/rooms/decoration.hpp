#pragma once

#include "skyland/room.hpp"



namespace skyland {



class Decoration : public Room {
public:

    using Room::Room;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
    }


    bool disallow_chimney() override
    {
        return true;
    }


    bool has_roof() override
    {
        return false;
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



}
