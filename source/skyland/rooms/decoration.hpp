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


    static bool unlocked_by_default()
    {
        // Should probably be false, as the whole purpose of decorations is to
        // reward the player for achievements.
        // // return true;

        return false;
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
