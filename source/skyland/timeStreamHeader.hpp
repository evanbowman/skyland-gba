#pragma once

#include "number/endian.hpp"
#include "number/numeric.hpp"



namespace skyland::time_stream {



using Timestamp = Microseconds;



namespace event {



struct Header
{
    HostInteger<Timestamp> timestamp_;
    u8 type_;
};



} // namespace event



} // namespace skyland::time_stream
