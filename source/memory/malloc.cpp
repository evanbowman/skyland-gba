////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "malloc.hpp"



namespace malloc_compat
{



static Heap* bound_heap_;



Heap::Heap()
{
    parent_ = bound_heap_;
    bound_heap_ = this;
}



Heap::~Heap()
{
    bound_heap_ = parent_;
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
        Platform::fatal(
            format("free misaligned address! %", (intptr_t)addr).c_str());
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



void* Heap::Sector::try_alloc(u32 size, u32 flags)
{
    if (size > (word_count + 1) * sizeof(Word)) {
        Platform::fatal(
            format("allocation of % exceeds max size!", size).c_str());
    }

    const bool permanent = flags & smf_permanent;

    int required_words = size / sizeof(Word);
    if (size % sizeof(Word)) {
        ++required_words;
    }
    if (not permanent) {
        // In permanent alloc mode, allocation size isn't stored, saving a few
        // bytes.
        ++required_words; // +1 for allocation size.
    }

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

                if (not smf_permanent) {
                    ++start; // inc result pointer, to skip over the size param.
                }

                for (int ii = i; ii - i < required_words; ++ii) {
                    taken_.set(ii, true);
                }

                return start;
            }
        }
    }

    return nullptr;
}



} // namespace malloc_compat



extern "C" {



#ifdef __GBA__
void* skyland_malloc(u32 sz, u32 flags)
{
    using namespace malloc_compat;

    if (not bound_heap_) {
        return nullptr;
    }

    for (auto& s : bound_heap_->sectors_) {
        if (auto p = s.try_alloc(sz, flags)) {
            return p;
        }
    }

    bound_heap_->sectors_.emplace_back();

    for (auto& s : bound_heap_->sectors_) {
        if (auto p = s.try_alloc(sz, flags)) {
            return p;
        }
    }

    return nullptr;
}



void skyland_free(void* ptr)
{
    using namespace malloc_compat;

    for (auto& s : bound_heap_->sectors_) {
        if (s.contains_address(ptr)) {
            s.free((Heap::Sector::Word*)ptr);
            return;
        }
    }

    Platform::fatal(format("invalid address passed to free! %", (intptr_t)ptr));
}



// NOTE: libstdc++ calls malloc during initialization of some global objects. I
// consider this to be excessively stupid, but what can I do. Better to define a
// null version of malloc than fallback to the version from newlib.
void* malloc(size_t sz)
{
    return nullptr;
}



void free(void* ptr)
{
    Platform::fatal("libc free callled?!");
}



#endif // __GBA__
}
