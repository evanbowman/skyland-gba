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
