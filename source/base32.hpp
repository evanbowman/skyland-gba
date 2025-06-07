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

#include "containers/vector.hpp"



namespace base32
{



Vector<char> encode(Vector<char>& input, const char* alphabet = nullptr);



Vector<char> decode(Vector<char>& input, const char* alphabet = nullptr);



} // namespace base32
