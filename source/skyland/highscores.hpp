#pragma once

#include "number/endian.hpp"


namespace skyland {


struct Highscores {
    static const int count = 6;
    HostInteger<u32> values_[count];
};


} // namespace skyland
