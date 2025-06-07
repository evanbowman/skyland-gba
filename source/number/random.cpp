////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "random.hpp"


rng::LinearGenerator rng::critical_state;
rng::LinearGenerator rng::utility_state;


rng::Value rng::get(LinearGenerator& gen)
{
    gen = 1664525 * gen + 1013904223;
    return (gen >> 16) & 0x7FFF;
}
