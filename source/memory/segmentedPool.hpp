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



private:
    Buffer<DynamicMemory<Pool>, pool_count> pools_;
};
