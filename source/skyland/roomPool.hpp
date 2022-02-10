#pragma once

#include "bulkAllocator.hpp"
#include "memory/pool.hpp"
#include "skyland/room.hpp"
#include <memory>



namespace skyland {



namespace room_pool {



#if defined(__GBA__) or defined(__NDS__)
static constexpr const int max_room_size = 52;
#else
static constexpr const int max_room_size = 128;
#endif
static constexpr const int pool_capacity = 140;
static constexpr const int alignment = 8;



struct RoomPools {
public:
    // NOTE: each room occupies 52 bytes, plus a four byte freelist pointer, so
    // 56 bytes, i.e. ~35 rooms fit in each pool, as pools are reified as
    // scratch buffers (2k pages).
    static const auto rooms_per_pool = 35;
    static const auto pool_count = pool_capacity / rooms_per_pool;

    static_assert(pool_count < 5,
                  "Just a sanity check. We want to understand memory usage "
                  "in the room pool.");


    static constexpr int capacity()
    {
        return pool_count * rooms_per_pool;
    }


    using RoomPool = Pool<max_room_size, rooms_per_pool, entity_pool_align>;


    void init(Platform& pfrm)
    {
        for (u32 i = 0; i < pools_.capacity(); ++i) {
            pools_.push_back(allocate_dynamic<RoomPool>(pfrm));
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
            if (r >= pl->cells().begin() and r < pl->cells().end()) {
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
