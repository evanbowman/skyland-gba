#pragma once

#include "number/int.h"
#include "number/endian.hpp"



namespace flash_filesystem
{



struct Record
{
    enum InvalidateStatus
    {
        // A flash erase will set all values in a sector to 0xffff. This is
        // simply how flash storage works. A flash write can only change bits
        // from one to zero, an erase operation sets all bits in a flash sector
        // to one.
        valid = 0xff,
        invalid = 0x00
    };

    InvalidateStatus invalidate_;

    struct FileInfo
    {
        // Sanity check byte. In case a cosmic ray flipped a bit or something.
        u8 crc_;
        u8 flags_; // unused
        u8 name_length_;
        host_u16 data_length_;


        // NOTE: appended data:
        //
        // char name_[name_length_];
        // char data_[data_length_];
    } file_info_;
};




}
