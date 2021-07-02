#pragma once

#include "number/endian.hpp"


namespace skyland {


struct Highscores {
    char magic_[2] = {'H', 'S'};

    static const int count = 6;
    HostInteger<u32> values_[count];
};


}
