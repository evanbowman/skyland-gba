////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "number/int.h"


enum class Key : u8 {
    // The main interact button, e.g. the A button on Nintendo consoles.
    action_1,

    // The equivalent of the B button on Nintendo consoles. Should be used
    // typically to cancel out of menus.
    action_2,

    // Almost all platforms have at least two buttons! But in case there are a
    // couple more:
    action_3,
    action_4,


    start,
    select,
    left,
    right,
    up,
    down,

    // Left shoulder button.
    alt_1,

    // Right shoulder button.
    alt_2,


    count,
    null
};
