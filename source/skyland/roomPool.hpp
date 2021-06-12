#pragma once

#include <memory>
#include "memory/pool.hpp"
#include "skyland/room.hpp"


namespace skyland {


namespace room_pool {
    static constexpr const int max_room_size = 64;
    static constexpr const int pool_capacity = 32;


    using _Pool = Pool<max_room_size, pool_capacity>;

    extern _Pool* pool_;


    inline void deleter(Room* room)
    {
        if (room) {
            room->~Room();
            pool_->post(reinterpret_cast<byte*>(room));
        }
    }


    template <typename T, typename... Args>
    RoomPtr<T> alloc(Args&&... args)
    {
        static_assert(sizeof(T) <= max_room_size);

        if (pool_ == nullptr) {
            return {nullptr, deleter};
        }

        if (auto mem = pool_->get()) {
            new (mem) T(std::forward<Args>(args)...);

            return {reinterpret_cast<T*>(mem), deleter};
        }

        return {nullptr, deleter};
    }


}


}
