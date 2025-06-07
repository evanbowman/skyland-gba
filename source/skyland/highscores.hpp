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

#include "number/endian.hpp"


namespace skyland
{


struct Highscores
{
    static const int count = 6;
    HostInteger<u32> values_[count];
    HostInteger<u32> highest_score_play_seconds_;
    u8 highest_score_multiplier_used_;
};


} // namespace skyland
