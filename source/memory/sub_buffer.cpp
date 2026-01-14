////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "sub_buffer.hpp"
#include "pool.hpp"


#ifdef __GBA__
static constexpr const int sub_buffers_per_pool = 7;
#else
// NOTE: due to larger underlying scratch buffers on desktop cpus.
static constexpr const int sub_buffers_per_pool = 26;
#endif


struct SubBufferPool
{
    SubBufferPool(const char* tag) : pool_(tag)
    {
    }

    ObjectPool<SubBufferControlBlock, sub_buffers_per_pool> pool_;

    Optional<DynamicMemory<SubBufferPool>> next_;
};



Optional<DynamicMemory<SubBufferPool>> sub_buffer_pools;



SubBufferPool* find_sub_buffer_pool(ScratchBuffer* ptr)
{
    auto current = &*sub_buffer_pools;
    while (true) {
        if (current->memory_.get() == ptr) {
            return &(**current);
        }
        if (not(*current)->next_) {
            return nullptr;
        }
        current = &(*(*current)->next_);
    }
}



void sub_buffer_memory_diagnostics(
    ScratchBuffer* backing_buffer,
    Function<4 * sizeof(void*), void(const char*)> cb)
{
    int sub_num = 0;
    if (auto pl = find_sub_buffer_pool(backing_buffer)) {
        for (auto& cell : pl->pool_.cells()) {
            if (not pl->pool_.is_freed(&cell)) {
                StringBuffer<96> output;
                output = "    -> ";
                output += format("[%/%]: ", sub_num + 1, sub_buffers_per_pool)
                              .c_str();
                output +=
                    ((SubBufferControlBlock*)cell.mem_.data())->data_.tag_;
                cb(output.c_str());
                ++sub_num;
            }
        }
    }
}



bool is_sub_buffer_pool(ScratchBuffer* ptr)
{
    return find_sub_buffer_pool(ptr);
}



SubBufferPtr make_sub_buffer(const SubBuffer::Tag& tag, u32 zero_fill_size)
{
    auto alloc_subpool = [] {
        const char* tag = "sub-buffer-pool";
        return allocate_fast<SubBufferPool>(tag, tag);
    };

    if (not sub_buffer_pools) {
        sub_buffer_pools = alloc_subpool();
    }

    auto current = &*sub_buffer_pools;
    while (true) {
        if (auto mem = (*current)->pool_.alloc()) {
            auto free_mem = [](SubBufferControlBlock* mem) {
                if (not sub_buffer_pools) {
                    LOGIC_ERROR();
                }
                auto current = &*sub_buffer_pools;
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
                                sub_buffer_pools = std::move((*current)->next_);
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
            mem->data_.tag_ = tag;
            mem->finalizer_hook_ = free_mem;

            static const int wordsize = sizeof(void*);

            if (zero_fill_size > SUB_BUFFER_SIZE) {
                PLATFORM.fatal("buffer overfill");
            }

            auto buf = SubBufferPtr(mem);
            if ((intptr_t)buf->data_ % wordsize == 0 and
                zero_fill_size % wordsize == 0) {
                PLATFORM.memset_words(buf->data_, 0, zero_fill_size / wordsize);
            } else {
                memset(buf->data_, 0, zero_fill_size);
            }

            return buf;
        }
        if (not(*current)->next_) {
            (*current)->next_ = alloc_subpool();
        }
        current = &(*(*current)->next_);
    }
}



SubBufferMemory::PtrType SubBufferMemory::create(SubBuffer::Tag t,
                                                 u32 zero_fill_size)
{
    return make_sub_buffer(t, zero_fill_size);
}
