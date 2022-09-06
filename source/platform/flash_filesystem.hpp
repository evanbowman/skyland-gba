////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


// NOTE: to compile the unit tests:
// g++ -std=c++17 flash_filesystem.cpp -I /home/evan/skyland/source/ -g3 -D__FAKE_VECTOR__



#pragma once

#include "function.hpp"
#include "memory/buffer.hpp"
#include "number/endian.hpp"
#include "number/int.h"
#include "string.hpp"



class Platform;

#ifndef __GBA__
#include <fstream>
#include <iostream>
#include <vector>
inline void info(Platform& pfrm, const StringBuffer<200>& msg)
{
    std::cout << msg.c_str() << std::endl;
}
#ifdef __FAKE_VECTOR__
template <typename T> using Vector = Buffer<T, 32000>;
#else
#include "containers/vector.hpp"
#endif
#else
#include "containers/vector.hpp"
#endif // __GBA__



namespace flash_filesystem
{



#ifndef FS_MAX_PATH
#define FS_MAX_PATH 86
#endif



static constexpr const int max_path = FS_MAX_PATH;



struct Statistics
{
    u16 bytes_used_;
    u16 bytes_available_;
};



Statistics statistics(Platform& pfrm);



enum InitStatus {
    // Newly initialized
    initialized,

    // Previously initialized
    already_initialized,

    failed,
};



InitStatus initialize(Platform& pfrm, u32 offset);



bool store_file_data(Platform&, const char* path, Vector<char>& data);



u32 read_file_data(Platform&, const char* path, Vector<char>& output);



u32 file_size(Platform&, const char* path);



inline u32
read_file_data_text(Platform& pfrm, const char* path, Vector<char>& output)
{
    auto read = read_file_data(pfrm, path, output);
    output.push_back('\0');

    return read;
}



inline bool
store_file_data_text(Platform& pfrm, const char* path, Vector<char>& data)
{
    data.pop_back();
    auto result = store_file_data(pfrm, path, data);
    data.push_back('\0');

    return result;
}



inline u32
read_file_data_binary(Platform& pfrm, const char* path, Vector<char>& output)
{
    return read_file_data(pfrm, path, output);
}



inline bool
store_file_data_binary(Platform& pfrm, const char* path, Vector<char>& data)
{
    return store_file_data(pfrm, path, data);
}



inline bool
store_file_data(Platform& pfrm, const char* path, const char* ptr, u32 length)
{
    Vector<char> buffer;
    for (u32 i = 0; i < length; ++i) {
        buffer.push_back(ptr[i]);
    }
    buffer.push_back('\0');

    return store_file_data_text(pfrm, path, buffer);
}



void walk(Platform& pfrm,
          Function<8 * sizeof(void*), void(const char*)> callback);



template <typename F>
void walk_directory(Platform& pfrm, const char* directory, F callback)
{
    walk(pfrm, [callback, directory](const char* path) {
        auto remainder =
            starts_with(directory, StringBuffer<FS_MAX_PATH>(path));
        if (remainder) {
            callback(remainder);
        }
    });
}



void unlink_file(Platform& pfrm, const char* path);



bool file_exists(Platform& pfrm, const char* path);



void destroy(Platform& pfrm);



} // namespace flash_filesystem
