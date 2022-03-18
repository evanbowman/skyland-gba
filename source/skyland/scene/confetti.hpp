#pragma once


#include "allocator.hpp"



namespace skyland {



struct Confetti
{
    Float x_;
    Float y_;
    Float speed_;
    int angle_;
    Float gravity_;
    u8 img_;
    u8 clr_;
    u8 kf_;
    u8 anim_;
    u8 fall_slower_;
};



using ConfettiBuffer = Buffer<Confetti, 60>;



} // namespace skyland
