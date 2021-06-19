#pragma once



#include "bulkAllocator.hpp"



namespace skyland {



class Island;



using PathBuffer = Buffer<Vec2<u8>, 512>;
using Path = DynamicMemory<PathBuffer>;


std::optional<Path> find_path(Platform& pfrm,
                              Island* island,
                              const Vec2<u8>& start,
                              const Vec2<u8>& end);



} // namespace skyland
