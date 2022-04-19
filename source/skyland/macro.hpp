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



class Platform;



namespace skyland::macro::terrain
{



enum class Type {
    air,
    building,
    rock_edge,
    water,
    rock_stacked,
    masonry,
    selector,
    water_slant_a,
    water_slant_b,
    water_slant_c,
    water_slant_d,
    count,
};



struct Block
{
    u8 type_ : 6;

    u8 shadowed_ : 1;
    u8 repaint_ : 1;

    u8 data_;

    Block() : shadowed_(true), repaint_(true)
    {
    }
};
static_assert(sizeof(Block) == 2);



class Sector
{
public:
    void set_block(const Vec3<u8>& coord, Type type);


    const Block& get_block(const Vec3<u8>& coord) const;


    void rotate();


    void update(Platform& pfrm);


    void render(Platform& pfrm);


    static const int z_limit = 9;


    Vec3<u8> cursor() const
    {
        return cursor_;
    }


    u16 population_ = 0;


    Vec2<s8> coordinate();


    void set_cursor(const Vec3<u8>& pos);


    bool changed_ = false;


    void serialize(u8* output);


private:
    void shadowcast();

    bool shrunk_ = false;

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
    };


    State() :
        data_(allocate_dynamic<Data>("macrocosm-data"))
    {
    }

    DynamicMemory<Data> data_;
};



}
