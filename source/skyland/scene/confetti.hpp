////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "allocator.hpp"



namespace skyland
{



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
