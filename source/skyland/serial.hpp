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
#include "string.hpp"



class Platform;



namespace skyland
{



class Island;



using SerialString = StringBuffer<1600>;



DynamicMemory<SerialString> serialize(Island& island);



} // namespace skyland
