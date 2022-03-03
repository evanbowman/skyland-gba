#pragma once

#include "memory/rc.hpp"


#if defined(__GBA__) or defined(__NDS__)
#define SCRATCH_BUFFER_SIZE 2000
#else
#define SCRATCH_BUFFER_SIZE 8000
#endif


struct ScratchBuffer
{
    // NOTE: do not make any assumptions about the alignment of the data_
    // member.
    char data_[SCRATCH_BUFFER_SIZE];

    using Tag = const char*;
    Tag tag_;
};

static constexpr const int scratch_buffer_count = 87;
using ScratchBufferPtr =
    Rc<ScratchBuffer,
       PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>>;
