////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "platform/scratch_buffer.hpp"
#include "memory/sub_buffer.hpp"


template <typename T, typename M = ScratchBufferMemory> class Vector;

template <typename T>
using CompactVector = Vector<T, SubBufferMemory>;
