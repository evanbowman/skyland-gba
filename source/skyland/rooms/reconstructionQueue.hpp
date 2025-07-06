#pragma once

#include "memory/tinyBuffer.hpp"
#include "skyland/metaclassIndex.hpp"



namespace skyland
{



struct ReconstructionQueueEntry
{
    u8 block_metaclass_;
    u8 x_ : 4;
    u8 y_ : 4;
};



static constexpr const int reconstruction_queue_size = 15;



using ReconstructionQueue =
    TinyBuffer<ReconstructionQueueEntry, reconstruction_queue_size>;



} // namespace skyland
