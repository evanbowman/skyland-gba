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
