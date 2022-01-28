#pragma once

#include "number/endian.hpp"



// The data format for our dlc data transfer protocol. The platform library
// implements the minimum platform specific behavior necessary for downloading a
// big binary blob. Then, the game code is responsible for parsing the dlc
// data.



namespace skyland::dlc::archive {



struct Header {
    host_u16 file_count_;
};



struct FileDescription {
    u8 name_length_;
    host_u16 file_size_;

    // char name_[name_length_];
    // char data_[file_size_];
};



}
