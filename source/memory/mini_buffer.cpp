////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "mini_buffer.hpp"
#include "allocator.hpp"
#include "pool.hpp"



static constexpr const int mini_buffers_per_pool =
    (SCRATCH_BUFFER_SIZE / (sizeof(MiniBufferControlBlock) + sizeof(void*))) - 1;



struct MiniBufferPool
{
    MiniBufferPool(const char* tag) : pool_(tag)
    {
    }

    ObjectPool<MiniBufferControlBlock, mini_buffers_per_pool> pool_;
    Optional<DynamicMemory<MiniBufferPool>> next_;
};



Optional<DynamicMemory<MiniBufferPool>> mini_buffer_pools;



MiniBufferPtr make_mini_buffer(u32 zero_fill_size)
{
    auto alloc_minipool = [] {
        const char* tag = "mini-buffer-pool";
        return allocate_fast<MiniBufferPool>(tag, tag);
    };

    if (not mini_buffer_pools) {
        mini_buffer_pools = alloc_minipool();
    }

    auto current = &*mini_buffer_pools;
    while (true) {
        if (auto mem = (*current)->pool_.alloc()) {
            auto free_mem = [](MiniBufferControlBlock* mem) {
                if (not mini_buffer_pools) {
                    LOGIC_ERROR();
                }
                auto current = &*mini_buffer_pools;
                decltype(current) prev = nullptr;
                while (true) {
                    if ((*current)->pool_.contains((u8*)mem)) {
                        (*current)->pool_.free(mem);
                        if ((*current)->pool_.pooled_element_count() ==
                            (*current)->pool_.remaining()) {
                            // Unlink current pool from chain if all memory has
                            // been returned to it.
                            if (prev) {
                                (*prev)->next_ = std::move((*current)->next_);
                            } else {
                                // Unlinking the head of the list
                                mini_buffer_pools = std::move((*current)->next_);
                            }
                        }
                        return;
                    }
                    if ((*current)->next_) {
                        prev = current;
                        current = &(*(*current)->next_);
                    } else {
                        LOGIC_ERROR();
                    }
                }
            };
            mem->finalizer_hook_ = free_mem;

            static const int wordsize = sizeof(void*);

            if (zero_fill_size > SUB_BUFFER_SIZE) {
                PLATFORM.fatal("buffer overfill");
            }

            auto buf = MiniBufferPtr(mem);
            if (zero_fill_size) {
                if ((intptr_t)buf->data_ % wordsize == 0 and
                    zero_fill_size % wordsize == 0) {
                    PLATFORM.memset_words(
                        buf->data_, 0, zero_fill_size / wordsize);
                } else {
                    memset(buf->data_, 0, zero_fill_size);
                }
            }

            return buf;
        }
        if (not(*current)->next_) {
            (*current)->next_ = alloc_minipool();
        }
        current = &(*(*current)->next_);
    }
}
