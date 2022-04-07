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


#include "function.hpp"
#include "number/endian.hpp"
#include "number/int.h"



namespace filesystem
{



struct Root
{
    char magic_[4];
    host_u32 file_count_;
};



struct FileHeader
{
    char path_[64]; // Must be null-terminated.
    host_u32 size_;
};



using NullTerminatedString = const char*;
using FileContents = NullTerminatedString;
using FilePath = NullTerminatedString;



bool is_mounted();



FileContents load(FilePath path);


void walk(Function<8 * sizeof(void*), void(const char* path)> callback);



} // namespace filesystem
