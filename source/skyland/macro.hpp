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
    void set_block(const Vec3<u8> coord, Type type);


    const Block& get_block(const Vec3<u8> coord);


    void rotate();


    void update(Platform& pfrm);


    void render(Platform& pfrm);


    static const int z_limit = 9;


private:
    void shadowcast();


    u8 x_;
    u8 y_;

    bool changed_ = false;

    u8 z_view_ = z_limit;

    Block blocks_[z_limit][8][8]; // (z, x, y)

    Vec3<u8> cursor_;
};



} // namespace skyland::macro::terrain



namespace skyland::macro
{


struct State
{
    State() :
        sector_(allocate_dynamic<macro::terrain::Sector>("macrocosm-sector"))
    {
    }

    DynamicMemory<macro::terrain::Sector> sector_;
};



}
