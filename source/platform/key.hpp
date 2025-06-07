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
