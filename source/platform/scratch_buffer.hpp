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

#include "function.hpp"
#include "memory/rc.hpp"



#if defined(__GBA__) or defined(__NDS__)
#define SCRATCH_BUFFER_SIZE 2048
#else
#define SCRATCH_BUFFER_SIZE 8192
#endif


struct ScratchBuffer
{
    // NOTE: do not make any assumptions about the alignment of the data_
    // member.
    char data_[SCRATCH_BUFFER_SIZE];

    using Tag = const char*;
    Tag tag_;
};

static constexpr const int scratch_buffer_count = 83;
using ScratchBufferPtr =
    Rc<ScratchBuffer,
       PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>>;



ScratchBufferPtr make_scratch_buffer(const ScratchBuffer::Tag& tag);



ScratchBufferPtr make_zeroed_sbr(const ScratchBuffer::Tag& tag,
                                 u32 zero_fill_size = SCRATCH_BUFFER_SIZE);



int scratch_buffers_remaining();



int scratch_buffers_in_use();



// An emergency function to invoke when the system runs out of scratch
// buffers. This function should, if possible, drop any non-essential references
// to scratch buffers.
void set_scratch_buffer_oom_handler(
    Function<4 * sizeof(void*), void()> callback);



class Platform;
void scratch_buffer_memory_diagnostics();
void scratch_buffer_dump_sector(int sector);
