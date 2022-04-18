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

#include "number/int.h"



class Platform;



namespace skyland::macro
{



namespace terrain
{



struct Block
{
    u8 type_ : 6;
    u8 shadowed_ : 1;
    u8 unused_ : 1;
    u8 data_;
};
static_assert(sizeof(Block) == 2);



struct Chunk
{
    u8 x_;
    u8 y_;

    Block blocks_[8][8][8]; // (z, x, y)


    void rotate();
    void shadowcast();
};



} // namespace terrain



void render(Platform& pfrm, terrain::Chunk& chunk);



} // namespace skyland::macro
