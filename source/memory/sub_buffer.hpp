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
#include "bitvector.hpp"


#define SUB_BUFFER_SIZE 268


struct SubBuffer
{
    static constexpr const u32 size = SUB_BUFFER_SIZE;

    char data_[SUB_BUFFER_SIZE];

    using Tag = ScratchBuffer::Tag;
    Tag tag_;
};


struct SubBufferControlBlock
{
    SubBuffer data_;

    SubBuffer& data()
    {
        return data_;
    }

    void (*finalizer_hook_)(SubBufferControlBlock*);
    u16 strong_count_ = 0;
    u16 weak_count_ = 0;
};


using SubBufferPtr = Rc<SubBuffer, SubBufferControlBlock>;


SubBufferPtr make_sub_buffer(const SubBuffer::Tag& tag);



struct SubBufferMemory
{
    using Type = SubBuffer;
    using PtrType = SubBufferPtr;
    static PtrType create(SubBuffer::Tag t);
};
