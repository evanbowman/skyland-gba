#pragma once


#include "string.hpp"
#include "vector.hpp"


using StringVector =
    StringAdapter<std::numeric_limits<u32>::max(), Vector<char>>;
