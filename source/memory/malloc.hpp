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



// I didn't add a malloc implementation until nearly the end of this
// project. Only exists in case I want to leverage external
// dependencies. Skyland does not use malloc anywhere. NOTE: max allocation size
// is slightly less than SCRATCH_BUFFER_SIZE.
//
// If you need a malloc implementation, create a Heap object on the stack, then
// you may call malloc and free until Heap goes out of scope. Two Heap instances
// may not be created at once. This isn't a fast malloc or even a halfway decent
// malloc. It's the best malloc that I could write in half an hour. I'm not
// using any open-source malloc, as my pooled allocation architecture does not
// support contiguous allocations larger than 2kb.
//
// P.S. I'm not using the weakly linked malloc symbol for my malloc fuction
// name. I've seen cases where the compiler messes up and calls the malloc
// version from newlib, along with my replacement version of free, and vice
// versa.



extern "C" {


// Allocate chunks of memory up to the engine's page size. Generally, may not
// exceed 2kb. Slow. Intended for the rare instances when we actually need to
// malloc something.
void* skyland_malloc(size_t sz);
void skyland_free(void* ptr);
}



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


        void* try_alloc(u32 size);
    };

    using Sectors = Vector<Sector>;

    Sectors sectors_;
    Heap* parent_ = nullptr;
};



} // namespace malloc_compat
