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



struct SubBufferPool
{
    SubBufferPool(const char* tag) : pool_(tag)
    {
    }

    ObjectPool<SubBufferControlBlock, 7> pool_;

    Optional<DynamicMemory<SubBufferPool>> next_;
};



Optional<DynamicMemory<SubBufferPool>> sub_buffer_pools;



SubBufferPtr make_sub_buffer(const SubBuffer::Tag& tag)
{
    auto alloc_subpool = [] {
        const char* tag = "sub-buffer-pool";
        return allocate_dynamic<SubBufferPool>(tag, tag);
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
            return SubBufferPtr(mem);
        }
        if (not (*current)->next_) {
            (*current)->next_ = alloc_subpool();
        }
        current = &(*(*current)->next_);
    }
}



SubBufferMemory::PtrType SubBufferMemory::create(SubBuffer::Tag t)
{
    return make_sub_buffer(t);
}
