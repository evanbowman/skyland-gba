
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
    static const int data_alignment = alignof(void*);

    alignas(data_alignment) char data_[SCRATCH_BUFFER_SIZE];

    using Tag = const char*;
    Tag tag_;
};

static constexpr const int scratch_buffer_count = 84;
using ScratchBufferPtr =
    Rc<ScratchBuffer,
       PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>>;



ScratchBufferPtr make_scratch_buffer(const ScratchBuffer::Tag& tag,
                                     bool zero_fill = true);



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
