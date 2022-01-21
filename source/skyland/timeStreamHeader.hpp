#pragma once

#include "number/numeric.hpp"
#include "number/endian.hpp"



namespace skyland::time_stream {



using Timestamp = Microseconds;



namespace event {



struct Header {
    HostInteger<Timestamp> timestamp_;
    u8 type_;
};



}



}
