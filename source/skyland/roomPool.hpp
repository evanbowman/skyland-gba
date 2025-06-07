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
#include "memory/segmentedPool.hpp"
#include "skyland/room.hpp"



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



using RoomPools = SegmentedPool<max_room_size, pool_capacity, 26, 8>;

static_assert(RoomPools::segment_count() < 7);



} // namespace room_pool



} // namespace skyland
