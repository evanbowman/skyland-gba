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
#include "pool.hpp"



template <u32 max_obj_size, u32 objs, u32 objs_per_subpool, u32 align>
class SegmentedPool
{
public:
    using Pool = ::Pool<max_obj_size, objs_per_subpool, align>;

    static constexpr const auto pool_count = objs / objs_per_subpool;


    static constexpr int capacity()
    {
        static_assert(objs == pool_count * objs_per_subpool);
        return objs;
    }


    static constexpr int segment_count()
    {
        return pool_count;
    }


    void create(const char* pool_label)
    {
        while (not pools_.full()) {
            pools_.push_back(allocate_dynamic<Pool>(pool_label, pool_label));
        }
    }


    void destroy()
    {
        for (auto& pool : pools_) {
            if (pool->pooled_element_count() not_eq
                pool->pooled_element_remaining()) {
                Platform::fatal("attempt to destroy pool with outstanding "
                                "references.");
            }
        }
        pools_.clear();
    }


    void* alloc()
    {
        for (auto& pl : pools_) {
            if (not pl->empty()) {
                return pl->alloc();
            }
        }
        return nullptr;
    }


    void free(void* e)
    {
        for (auto& pl : pools_) {
            if (e >= (void*)pl->cells().data() and
                e < (void*)(pl->cells().data() + pl->cells().size())) {
                pl->free((u8*)e);
                return;
            }
        }

        Platform::fatal("attempt to free entity not allocated from pool");
    }


    bool empty() const
    {
        for (auto& pl : pools_) {
            if (not pl->empty()) {
                return false;
            }
        }
        return true;
    }



private:
    Buffer<DynamicMemory<Pool>, pool_count> pools_;
};
