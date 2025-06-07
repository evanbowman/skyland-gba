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



inline u32 fnv32(const char* data, u32 len)
{
    u32 hash = 2166136261U, i;

    for (i = 0; i < len; i++) {
        hash = hash ^ (data[i]);
        hash = hash * 16777619;
    }

    return hash;
}
