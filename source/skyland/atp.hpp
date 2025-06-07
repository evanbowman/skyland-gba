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

#include "number/fixnum.hpp"


namespace skyland
{



using AttackTargetPriority = FixedPoint<8, s32>;
using ATP = AttackTargetPriority;



constexpr ATP operator"" _atp(long double value)
{
    return ATP((static_cast<float>(value)));
}



} // namespace skyland
