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
