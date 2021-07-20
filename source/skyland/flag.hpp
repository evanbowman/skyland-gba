#pragma once


#include "number/int.h"



class Platform;



namespace skyland {



class App;



struct FlagPixels {
    static const int width = 13;
    static const int height = 11;

    u8 pixels[width][height];
};



void vram_write_flag(Platform& pfrm, const FlagPixels& px);



void load_default_flag(Platform& pfrm, App& app);



} // namespace skyland
