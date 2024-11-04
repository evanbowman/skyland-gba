
#pragma once

#include "memory/tinyBuffer.hpp"
#include "number/numeric.hpp"



namespace skyland
{



struct PackedTarget
{
    u8 x_ : 4;
    u8 y_ : 4;


    static PackedTarget pack(Vec2<u8> t)
    {
        return {t.x, t.y};
    }


    RoomCoord coord() const
    {
        return {x_, y_};
    }
};


using TargetQueue = TinyBuffer<PackedTarget, 3>;



} // namespace skyland
