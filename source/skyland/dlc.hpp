////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "number/endian.hpp"



// The data format for our dlc data transfer protocol. The platform library
// implements the minimum platform specific behavior necessary for downloading a
// big binary blob. Then, the game code is responsible for parsing the dlc
// data.



namespace skyland::dlc::archive
{



struct Header
{
    host_u16 file_count_;
};



struct FileDescription
{
    u8 name_length_;
    host_u16 file_size_;

    // char name_[name_length_];
    // char data_[file_size_];
};



} // namespace skyland::dlc::archive
