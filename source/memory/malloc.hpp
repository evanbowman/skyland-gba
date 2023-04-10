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

#include "bitvector.hpp"
#include "containers/vector.hpp"
#include "malloc.h"



// Generally, skyland uses pooling extensively throughout the project. I only
// use malloc in obscure cases where I need to perform long-lived allocations,
// or integrate open source libraries dependent on a malloc implementation.



namespace malloc_compat
{



struct Heap
{
    Heap();
    Heap(const Heap&) = delete;
    ~Heap();


    struct Sector
    {
        static const int word_count = 480;

        struct Word
        {
            // NOTE: I was previously using max_align_t, but align() (defined in
            // allocator.hpp), seemingly cannot align sizes larger than the cpu
            // wordsize anyway.
            alignas(sizeof(void*)) u8 data_[sizeof(void*)];
        };

        Bitvector<word_count> taken_;
        alignas(void*) Word words_[word_count];


        bool contains_address(void* addr) const;


        bool empty() const;


        void free(Word* addr);


        void* try_alloc(u32 size, u32 flags);
    };

    using Sectors = Vector<Sector>;

    Sectors sectors_;
    Heap* parent_ = nullptr;
};



} // namespace malloc_compat
