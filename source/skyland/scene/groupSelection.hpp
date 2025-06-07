////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "memory/buffer.hpp"
#include "skyland/coord.hpp"
#include "skyland/metaclassIndex.hpp"



namespace skyland
{



struct GroupSelection
{
    RoomCoord sel_tl_;
    RoomCoord sel_bl_;
    RoomCoord sel_tr_;
    RoomCoord sel_br_;

    RoomCoord anchor_;

    Optional<MetaclassIndex> room_mti_;

    Buffer<RoomCoord, 90> rooms_;
};



} // namespace skyland
