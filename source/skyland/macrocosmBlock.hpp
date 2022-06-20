////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "memory/buffer.hpp"
#include "number/int.h"
#include "systemString.hpp"



namespace skyland::macro
{


using Coins = s32;



}



namespace skyland::macro::terrain
{



enum class Type {
    air,
    building,
    __invalid,
    water_source,
    terrain,
    masonry,
    selector,
    wheat,
    indigo,
    madder,
    gold,
    workshop,
    water_slant_a,
    water_slant_b,
    water_slant_c,
    water_slant_d,
    light_source,
    windmill,
    windmill_stone_base,
    shellfish,
    port,
    potatoes,
    sunflowers,
    food, // Must not be constructed
    shrubbery,
    wool,
    saffron,
    ice,
    lava_source,
    lava_slant_a,
    lava_slant_b,
    lava_slant_c,
    lava_slant_d,
    volcanic_soil,
    cocoa,
    water_spread_downwards,
    // Why so many types of water blocks? When saving game state, we represent
    // each block with a single byte type id, so blocks must be essentially
    // stateless. Every variation in water behavior needs to use a different
    // block type. If you want to check more generally whether a block is water,
    // see Categories::fluid_water.
    //
    // water_spread_downwards represents a non-source water block where the
    // parent block lies above. The lateral spread blocks represent a non-source
    // water block where the parent block sits in the same z-plane, direction
    // depending on a,b,c,d.
    water_spread_laterally_a,
    water_spread_laterally_b,
    water_spread_laterally_c,
    water_spread_laterally_d,
    lava_spread_downwards,
    lava_spread_laterally_a,
    lava_spread_laterally_b,
    lava_spread_laterally_c,
    lava_spread_laterally_d,
    tea,
    lumber,
    basalt,
    arch,
    sand,
    crystal,
    marble,
    marble_top,
    scaffolding,
    tulips,
    pearls,
    road_ns,
    road_we,
    honey,

    count,
};



struct Commodity
{
    enum Type : u8 {
        indigo,
        rose_madder,
        shellfish,
        sunflowers,
        food,
        wool,
        saffron,
        cocoa,
        tea,
        lumber,
        tulips,
        pearls,
        honey,
    };
    Type type_;
    bool imported_ = false;
    u16 supply_;

    static Coins value(Type t);
    SystemString name() const;
};



struct Stats
{
    s16 food_ = 0;
    s16 food_exports_ = 0;
    s16 housing_ = 0;
    s16 employment_ = 0;
    s16 happiness_ = 0;

    Buffer<Commodity, 24> commodities_;
};



using Improvements = Buffer<Type, 32>;



struct Block
{
    u8 type_;

    // Make distinction between shadowed during the day and shadowed at night,
    // b/c the engine considers daytime lighting when determining crop yields.
    // Shadowed represents the current display state, while shadowed_day_
    // represents which blocks are shadowed during daytime.
    u8 shadowed_ : 1;
    u8 shadowed_day_ : 1;

    u8 data_ : 6;

    Stats stats() const;
    SystemString name() const;

    Improvements improvements() const;


    Type type() const
    {
        return (Type)type_;
    }
};
static_assert(sizeof(Block) == 2);
static_assert(std::is_standard_layout<Block>());
static_assert(std::is_trivially_constructible<Block>());



} // namespace skyland::macro::terrain
