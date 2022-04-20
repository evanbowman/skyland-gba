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

#include "allocator.hpp"
#include "number/int.h"
#include "systemString.hpp"



class Platform;



namespace skyland::macro
{
    using Coins = u32;
}



namespace skyland::macro::terrain
{



static const int food_consumption_factor = 2;



enum class Type {
    air,
    building,
    rock_edge,
    water,
    rock_stacked,
    masonry,
    selector,
    wheat,
    indigo,
    water_slant_a,
    water_slant_b,
    water_slant_c,
    water_slant_d,
    count,
};



struct Stats
{
    int food_ = 0;
    int housing_ = 0;
};



using Improvements = Buffer<Type, 10>;



Stats stats(Type t);
SystemString name(Type t);
std::pair<int, int> icons(Type t);
Improvements improvements(Type t);


Coins cost(Type t);



struct Block
{
    u8 type_ : 6;

    u8 shadowed_ : 1;
    u8 repaint_ : 1;

    u8 data_;


    Stats stats() const;
    SystemString name() const;

    Improvements improvements() const;


    Type type() const
    {
        return (Type)type_;
    }


    Block() : shadowed_(true), repaint_(true)
    {
    }
};
static_assert(sizeof(Block) == 2);



class Sector
{
public:
    enum Orientation { north, east, south, west };


    void set_block(const Vec3<u8>& coord, Type type);


    const Block& get_block(const Vec3<u8>& coord) const;


    void rotate();


    void update();


    void advance(int years);


    Stats stats() const;


    void render(Platform& pfrm);


    static const int z_limit = 9;


    Vec3<u8> cursor() const
    {
        return cursor_;
    }


    using Population = float;


    Population population_ = 0;


    Float population_growth_rate() const;


    Vec2<s8> coordinate();


    void set_cursor(const Vec3<u8>& pos, bool lock_to_floor = true);


    bool changed_ = false;


    void serialize(u8* output);


    // Projected position of the cursor onto the frame buffer.
    u16 cursor_raster_pos() const;


    Orientation orientation() const
    {
        return orientation_;
    }


private:
    void shadowcast();

    bool shrunk_ = false;

    Orientation orientation_ = Orientation::north;

    Vec3<u8> cursor_;
    bool cursor_moved_ = false;

    // We keep a cache of screen tiles used by the cursor, as an
    // optimization. The cursor animation requires frequent redraw.
    Buffer<u16, 8> cursor_raster_tiles_;

    s8 x_;
    s8 y_;

    u8 z_view_ = z_limit;

    Block blocks_[z_limit][8][8]; // (z, x, y)
};



} // namespace skyland::macro::terrain



namespace skyland::macro
{


struct State
{
    struct Data
    {
        macro::terrain::Sector sector_;
        u16 year_ = 0;
        Coins coins_ = 0;
    };


    macro::terrain::Sector& sector()
    {
        return data_->sector_;
    }


    State() : data_(allocate_dynamic<Data>("macrocosm-data"))
    {
    }

    DynamicMemory<Data> data_;


    void advance(int elapsed_years);
};



} // namespace skyland::macro
