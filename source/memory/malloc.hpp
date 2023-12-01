////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
