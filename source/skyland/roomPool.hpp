#pragma once

#include "bulkAllocator.hpp"
#include "memory/pool.hpp"
#include "skyland/room.hpp"
#include <memory>



namespace skyland {



namespace room_pool {



#if defined(__GBA__) or defined(__NDS__)
static constexpr const int max_room_size = 64;
#else
static constexpr const int max_room_size = 128;
#endif
static constexpr const int pool_capacity = 155;
static constexpr const int alignment = 8;



struct RoomPools
{
public:
    // NOTE: each room occupies 52 bytes, plus a four byte freelist pointer, so
    // 56 bytes, i.e. ~35 rooms fit in each pool, as pools are reified as
    // scratch buffers (2k pages).
    static const auto rooms_per_pool = 26;
    static const auto pool_count = pool_capacity / rooms_per_pool;

    static_assert(pool_count < 6,
                  "Just a sanity check. We want to understand memory usage "
                  "in the room pool.");


    static constexpr int capacity()
    {
        return pool_count * rooms_per_pool;
    }


    using RoomPool = Pool<max_room_size, rooms_per_pool, entity_pool_align>;


    void init()
    {
        for (u32 i = 0; i < pools_.capacity(); ++i) {
            pools_.push_back(
                allocate_dynamic<RoomPool>("room-pool-memory", "rooms"));
        }
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
