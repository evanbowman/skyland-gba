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
#include "containers/vector.hpp"
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



struct StorageOptions
{
    // Maybe you're thinking that compression shouldn't really be included in
    // the implementation of a filesystem library. But here's the thing. The
    // game now has a following of people who enjoy playing it. And if I want to
    // support compression for data stored in people's save files, I could
    // either create new copies of files with different paths, and perform the
    // conversions everwhere I save a file, moving old file data to new save
    // files (for backwareds compatibility), or, I could simply support
    // compression within the filesystem library, and no changes needed
    // throughout the codebase. It bloats the library a bit, but makes my life
    // way easier, because I don't need to keep track of different file paths
    // for old versions.
    bool use_compression_ = false;
};



bool store_file_data(Platform&,
                     const char* path,
                     Vector<char>& data,
                     const StorageOptions& = {});



u32 read_file_data(Platform&, const char* path, Vector<char>& output);



u32 file_size(Platform&, const char* path);



inline u32
read_file_data_text(Platform& pfrm, const char* path, Vector<char>& output)
{
    auto read = read_file_data(pfrm, path, output);
    output.push_back('\0');

    return read;
}



inline bool store_file_data_text(Platform& pfrm,
                                 const char* path,
                                 Vector<char>& data,
                                 const StorageOptions& opts = {})
{
    data.pop_back();
    auto result = store_file_data(pfrm, path, data, opts);
    data.push_back('\0');

    return result;
}



inline u32
read_file_data_binary(Platform& pfrm, const char* path, Vector<char>& output)
{
    return read_file_data(pfrm, path, output);
}



inline bool store_file_data_binary(Platform& pfrm,
                                   const char* path,
                                   Vector<char>& data,
                                   const StorageOptions& opts = {})
{
    return store_file_data(pfrm, path, data, opts);
}



template <typename T>
std::optional<T> read_file_blob(Platform& pfrm, const char* path)
{
    Vector<char> data;
    if (flash_filesystem::read_file_data_binary(pfrm, path, data)) {
        if (data.size() == sizeof(T)) {
            T result;
            for (u32 i = 0; i < data.size(); ++i) {
                ((u8*)&result)[i] = data[i];
            }
            return result;
        }
    }
    return std::nullopt;
}



template <typename T>
bool write_file_blob(Platform& pfrm, const char* path, const T& blob)
{
    Vector<char> data;

    for (u32 i = 0; i < sizeof blob; ++i) {
        data.push_back(((u8*)&blob)[i]);
    }

    return flash_filesystem::store_file_data_binary(pfrm, path, data);
}



inline bool store_file_data(Platform& pfrm,
                            const char* path,
                            const char* ptr,
                            u32 length,
                            const StorageOptions& opts = {})
{
    Vector<char> buffer;
    for (u32 i = 0; i < length; ++i) {
        buffer.push_back(ptr[i]);
    }
    buffer.push_back('\0');

    return store_file_data_text(pfrm, path, buffer, opts);
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
