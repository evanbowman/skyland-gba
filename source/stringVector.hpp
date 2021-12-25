#pragma once


#include "vector.hpp"
#include "string.hpp"


using StringVector =
    StringAdapter<std::numeric_limits<u32>::max(), Vector<char>>;
