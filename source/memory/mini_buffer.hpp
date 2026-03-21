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

#ifdef __GBA__
#define MINI_BUFFER_SIZE 32
#else
#define MINI_BUFFER_SIZE 64
#endif


struct MiniBuffer
{
    alignas(alignof(void*)) char data_[MINI_BUFFER_SIZE];
};


struct MiniBufferControlBlock
{
    MiniBuffer data_;

    MiniBuffer& data()
    {
        return data_;
    }

    void (*finalizer_hook_)(MiniBufferControlBlock*);
    u16 strong_count_ = 0;
    u16 weak_count_ = 0;
};


using MiniBufferPtr = Rc<MiniBuffer, MiniBufferControlBlock>;


MiniBufferPtr make_mini_buffer(u32 zero_fill_size);


struct MiniBufferMemory
{
    using Type = MiniBuffer;
    using PtrType = MiniBufferPtr;
    static PtrType create(u32 zero_fill_size = MINI_BUFFER_SIZE);
};
