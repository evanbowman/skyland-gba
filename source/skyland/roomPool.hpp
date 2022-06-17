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
#include "memory/pool.hpp"
#include "skyland/room.hpp"
#include <memory>



namespace skyland
{



namespace room_pool
{



#if defined(__GBA__) or defined(__NDS__)
static constexpr const int max_room_size = 64;
#else
static constexpr const int max_room_size = 128;
#endif
static constexpr const int pool_capacity = 181;
static constexpr const int alignment = 8;



struct RoomPools
{
public:
    // NOTE: each room occupies 52 bytes, plus a four byte freelist pointer, so
    // 56 bytes, i.e. ~35 rooms fit in each pool, as pools are reified as
    // scratch buffers (2k pages).
    static const auto rooms_per_pool = 26;
    static const auto pool_count = pool_capacity / rooms_per_pool;

    static_assert(pool_count < 7,
                  "Just a sanity check. We want to understand memory usage "
                  "in the room pool.");


    static constexpr int capacity()
    {
        return pool_count * rooms_per_pool;
    }


    using RoomPool = Pool<max_room_size, rooms_per_pool, entity_pool_align>;


    void create()
    {
        while (not pools_.full()) {
            pools_.push_back(
                allocate_dynamic<RoomPool>("room-pool-memory", "rooms"));
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


    void* get()
    {
        for (auto& pl : pools_) {
            if (not pl->empty()) {
                return pl->get();
            }
        }
        return nullptr;
    }


    bool empty()
    {
        for (auto& pl : pools_) {
            if (not pl->empty()) {
                return false;
            }
        }
        return true;
    }


    void post(void* r)
    {
        for (auto& pl : pools_) {
            if (r >= (void*)pl->cells().data() and
                r < (void*)(pl->cells().data() + pl->cells().size())) {
                pl->post((u8*)r);
                return;
            }
        }

        Platform::fatal("attempt to free entity not allocated from pool");
    }


private:
    Buffer<DynamicMemory<RoomPool>, pool_count> pools_;
};



} // namespace room_pool



} // namespace skyland
