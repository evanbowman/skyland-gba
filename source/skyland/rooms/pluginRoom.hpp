#pragma once

#include "skyland/room.hpp"



namespace skyland {


class PluginRoom : public Room {
public:
    PluginRoom(Island* parent, const Vec2<u8>& position, RoomMeta* metaclass)
        : Room(parent, nullptr, {1, 1}, position)
    {
        set_metaclass(metaclass);
    }


    void update(Platform&, App&, Microseconds delta) override
    {
    }


    void render_interior(u8 buffer[16][16]) override
    {
    }

    void render_exterior(u8 buffer[16][16]) override
    {
    }

    void plot_walkable_zones(bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }
};



} // namespace skyland
