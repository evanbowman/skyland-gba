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



namespace skyland::macro
{



namespace terrain
{



enum class Type {
    building,
    rock_edge,
    water,
    rock_stacked,
    masonry,
};



struct Block
{
    u8 type_ : 6;

    u8 shadowed_ : 1;
    u8 repaint_ : 1;

    u8 data_;

    Block() : shadowed_(true),
              repaint_(true)
    {
    }
};
static_assert(sizeof(Block) == 2);



struct Chunk
{
    u8 x_;
    u8 y_;

    static const int z_limit = 9;
    u8 z_view_ = z_limit;

    Block blocks_[z_limit][8][8]; // (z, x, y)


    void set_block(const Vec3<u8> coord, u8 type);


    Vec3<u8> cursor_;


    void rotate();
    void shadowcast();


    struct DepthNode
    {
        Vec3<u8> position_;
        u8 tile_ = 0;
        DepthNode* next_;
    };

    struct DepthBufferSlab
    {
        DepthNode* visible_[480];

        DepthBufferSlab()
        {
            for (auto& node : visible_) {
                node = nullptr;
            }
        }
    };

    struct DepthBuffer
    {
        // NOTE: DepthBufferSlab won't fit in a single allocation.
        DynamicMemory<DepthBufferSlab> depth_1_;
        DynamicMemory<DepthBufferSlab> depth_2_;

        BulkAllocator<18> depth_node_allocator_;

        DepthBuffer(Platform& pfrm)
            : depth_1_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer")),
              depth_2_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer")),
              depth_node_allocator_(pfrm)
        {
        }
    };

    std::optional<DepthBuffer> db_;
};



} // namespace terrain



void render(Platform& pfrm, terrain::Chunk& chunk);



} // namespace skyland::macro
