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
#include "number/numeric.hpp"



namespace skyland::time_stream
{



using Timestamp = Time;



namespace event
{



struct Header
{
    HostInteger<Timestamp> timestamp_;
    u8 type_;
};



} // namespace event



} // namespace skyland::time_stream
