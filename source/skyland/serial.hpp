#pragma once

#include "bulkAllocator.hpp"
#include "string.hpp"



class Platform;



namespace skyland {



class Island;



using SerialString = StringBuffer<980>;



DynamicMemory<SerialString> serialize(Platform&, Island& island);



} // namespace skyland
