#pragma once


#include "globals.hpp"



namespace skyland {
namespace room_pool {



inline void deleter(Room* room)
{
    if (room) {
        room->~Room();
        auto& pools = std::get<SkylandGlobalData>(globals()).room_pools_;
        pools.post(reinterpret_cast<u8*>(room));
    }
}



template <typename T, typename... Args> RoomPtr<T> alloc(Args&&... args)
{
    auto& pool = std::get<SkylandGlobalData>(globals()).room_pools_;

    static_assert(sizeof(T) <= max_room_size);
    static_assert(alignof(T) <= alignment);

    if (auto mem = pool.get()) {
        new (mem) T(std::forward<Args>(args)...);

        return RoomPtr<T>(reinterpret_cast<T*>(mem), deleter);
    }

    return {nullptr, deleter};
}



} // namespace room_pool
} // namespace skyland
