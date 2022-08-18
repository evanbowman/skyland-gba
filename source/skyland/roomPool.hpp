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
