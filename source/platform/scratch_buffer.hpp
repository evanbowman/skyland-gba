////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "function.hpp"
#include "memory/rc.hpp"


#if defined(__GBA__) or defined(__NDS__)
#define SCRATCH_BUFFER_SIZE 2000
#else
#define SCRATCH_BUFFER_SIZE 9000
#endif


struct ScratchBuffer
{
    // NOTE: do not make any assumptions about the alignment of the data_
    // member.
    char data_[SCRATCH_BUFFER_SIZE];

    using Tag = const char*;
    Tag tag_;
};

static constexpr const int scratch_buffer_count = 85;
using ScratchBufferPtr =
    Rc<ScratchBuffer,
       PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>>;



ScratchBufferPtr make_scratch_buffer(const ScratchBuffer::Tag& tag);



int scratch_buffers_remaining();



int scratch_buffers_in_use();



// An emergency function to invoke when the system runs out of scratch
// buffers. This function should, if possible, drop any non-essential references
// to scratch buffers.
void set_scratch_buffer_oom_handler(Function<16, void()> callback);



class Platform;
void scratch_buffer_memory_diagnostics(Platform& pfrm);
void scratch_buffer_dump_sector(Platform& pfrm, int sector);
