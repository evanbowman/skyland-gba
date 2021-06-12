#pragma once

#include "memory/buffer.hpp"
#include "room.hpp"
#include "bulkAllocator.hpp"


namespace skyland {

class Island {
public:
    Island(Platform& pfrm);

private:
    Buffer<Room*, 20> rooms_;
    Buffer<u8, 10> terrain_;
};


}
