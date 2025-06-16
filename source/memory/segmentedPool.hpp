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
        for (u32 i = 0; i < (u32)segment_count(); ++i) {
            pools_.push_back(allocate_dynamic<Pool>(pool_label, pool_label));
        }
    }


    void create(const char* pool_label, u32 segment_count)
    {
        destroy();

        if (segment_count > pools_.capacity()) {
            Platform::fatal("pool init request invalid");
        }

        for (u32 i = 0; i < segment_count; ++i) {
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

    using PoolsBuffer = Buffer<DynamicMemory<Pool>, pool_count>;
    PoolsBuffer& pools()
    {
        return pools_;
    }

private:
    PoolsBuffer pools_;
};
