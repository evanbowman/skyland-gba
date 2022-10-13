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


#include "malloc.hpp"



namespace malloc_compat
{



static Heap* bound_heap_;



Heap::Heap()
{
    if (bound_heap_) {
        Platform::fatal("attempt to create malloc heap when "
                        "one already exists!");
    }
    bound_heap_ = this;
}



Heap::~Heap()
{
    bound_heap_ = nullptr;
}



bool Heap::Sector::contains_address(void* addr) const
{
    return addr >= words_ and addr < words_ + word_count;
}



bool Heap::Sector::empty() const
{
    return taken_.empty();
}



void Heap::Sector::free(Word* addr)
{
    if (((intptr_t)addr) % sizeof(Word) not_eq 0) {
        Platform::fatal(format("free misaligned address! %",
                               (intptr_t)addr).c_str());
    }

    --addr; // fetch size from slot -1
    int count = *(int*)addr;
    int start_index = addr - words_;

    for (int i = start_index; i < start_index + count; ++i) {
        if (not taken_.get(i)) {
            Platform::fatal("heap corruption! (double free?)");
        }
        taken_.set(i, false);
    }
}



void* Heap::Sector::try_alloc(u32 size)
{
    if (size > (word_count + 1) * sizeof(Word)) {
        Platform::fatal(format("allocation of % exceeds max size!",
                               size).c_str());
    }

    int required_words = size / sizeof(Word);
    if (size % sizeof(Word)) {
        ++required_words;
    }
    ++required_words; // +1 for allocation size.

    for (int i = 0; i < word_count; ++i) {
        if (not taken_.get(i)) {
            bool found = true;
            for (int ii = i; ii < required_words; ++ii) {
                if (ii == word_count) {
                    return nullptr;
                }
                if (taken_.get(ii)) {
                    found = false;
                    break;
                }
            }

            if (found) {
                Word* start = &words_[i];
                // Store allocation size in the first slot.
                *(int*)(start) = required_words;
                ++start;

                for (int ii = i; ii < required_words; ++ii) {
                    taken_.set(ii, true);
                }

                return start;
            }
        }
    }

    return nullptr;
}



}



extern "C" {



void* malloc(size_t sz)
{
    using namespace malloc_compat;

    if (not bound_heap_) {
        return nullptr;
    }

    for (auto& s : bound_heap_->sectors_) {
        if (auto p = s.try_alloc(sz)) {
            return p;
        }
    }

    bound_heap_->sectors_.emplace_back();

    for (auto& s : bound_heap_->sectors_) {
        if (auto p = s.try_alloc(sz)) {
            return p;
        }
    }

    return nullptr;
}



void free(void* ptr)
{
    using namespace malloc_compat;

    for (auto& s : bound_heap_->sectors_) {
        if (s.contains_address(ptr)) {
            s.free((Heap::Sector::Word*)ptr);
            return;
        }
    }

    Platform::fatal("invalid address passed to free!");
}



}
