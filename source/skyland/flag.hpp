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


#include "number/int.h"
#include "platform/layer.hpp"



class Platform;



namespace skyland
{



class App;



struct FlagPixels
{
    static const int width = 13;
    static const int height = 11;

    u8 pixels[width][height];

    void save();
    void load();
    void load_custom(Layer layer, u16 offset);
};



void vram_write_flag(const FlagPixels& px, Layer layer);



void load_default_flag();



} // namespace skyland
