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


#include "globals.hpp"



namespace skyland
{
namespace room_pool
{



inline void deleter(Room* room)
{
    if (room) {
        room->~Room();
        auto& pools = globals().room_pools_;
        pools.free(reinterpret_cast<u8*>(room));
    }
}



template <typename T, typename... Args> RoomPtr<T> alloc(Args&&... args)
{
    auto& pool = globals().room_pools_;

    static_assert(sizeof(T) <= max_room_size);
    static_assert(alignof(T) <= alignment);

    if (auto mem = pool.alloc()) {
        new (mem) T(std::forward<Args>(args)...);

        return RoomPtr<T>(reinterpret_cast<T*>(mem), deleter);
    }

    return {nullptr, deleter};
}



} // namespace room_pool
} // namespace skyland
