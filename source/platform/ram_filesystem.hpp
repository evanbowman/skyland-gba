////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "containers/vector.hpp"
#include "number/endian.hpp"
#include "number/int.h"
#include "platform/platform.hpp"
#include <utility>



// A filesystem for Save RAM.
// Platform::read_save_data() and Platform::write_save_data() provide an
// abstraction for dealing with save memory. Technically, there's nothing
// platform-specific in this filesystem implementation, although desktop
// PCs have actual filesystems, so there's no reason to use this code for a
// hosted environment.



namespace ram_filesystem
{



enum {
    // Maximum supported path. Technically, the filesystem supports paths as
    // large as the block size, so you _could_ change this value, but that'd be
    // super wasteful. 64 bytes should be plenty.
    max_path = 64,

    // Block size used for the filesystem. Files will be sliced into ~200 byte
    // blocks. A larger block size will save a few bytes, as each byte stores a
    // two byte index representing the location of the next block in the file. A
    // smaller block size often ultimately saves more space, as a large block
    // size does not efficiently store small files.
    block_size = 200,
};



struct Statistics
{
    u16 blocks_used_;
    u16 blocks_available_;
};



Statistics statistics();



enum InitStatus {
    // Newly initialized
    initialized,

    // Previously initialized
    already_initialized,

    failed,
};



InitStatus initialize(int fs_begin_offset);



bool is_mounted();



void unlink_file(const char* path);



bool file_exists(const char* path);



void destroy();



size_t read_file_data_text(const char* path, Vector<char>& output);

bool store_file_data_text(const char* path, Vector<char>& data);


inline size_t read_file_data_binary(const char* path, Vector<char>& output)
{
    auto read = read_file_data_text(path, output);
    if (read > 0) {
        // This is binary data, ignore automatically-appended null byte.
        output.pop_back();
        return read;
    } else {
        return 0;
    }
}


inline bool store_file_data_binary(const char* path, Vector<char>& data)
{
    // NOTE: store_file_data assumes that the input is null terminated, and
    // ignores the final null character.
    data.push_back('\0');
    return store_file_data_text(path, data);
}



inline bool store_file_data(const char* path, const char* ptr, u32 length)
{
    Vector<char> buffer;
    for (u32 i = 0; i < length; ++i) {
        buffer.push_back(ptr[i]);
    }
    buffer.push_back('\0');

    return store_file_data_text(path, buffer);
}



int fs_offset();



int fs_contents_offset();



struct Root
{
    char magic_[4];
    host_u16 file_count_;
    host_u16 freelist_;
    // FileInfo file_info_[file_count_];
};



struct FileInfo
{
    host_u16 file_size_;
    host_u16 file_contents_;
};



struct FileContents
{
    struct Header
    {
        // NOTE: therefore, max filesystem size is 200 * 65535
        host_u16 next_;     // Zero if no more blocks
        host_u16 checksum_; // To check for file corruption
    } header_;

    static constexpr const auto capacity = block_size - sizeof header_;

    char data_[capacity];
};
static_assert(sizeof(FileContents) == block_size);


static_assert(max_path < FileContents::capacity);



Root load_root();



template <typename F> void walk_directory(const char* directory, F callback)
{
    walk([callback, directory](const char* path) {
        auto remainder = starts_with(directory, StringBuffer<max_path>(path));
        if (remainder) {
            callback(remainder);
        }
    });
}



template <typename F> void walk(F&& callback)
{
    if (not is_mounted()) {
        return;
    }

    auto root = load_root();

    auto offset = fs_offset() + sizeof(Root);

    for (int i = 0; i < root.file_count_.get(); ++i) {
        FileInfo info;
        PLATFORM.read_save_data(&info, sizeof info, offset);

        if (auto file = info.file_contents_.get()) {
            char path_buffer[max_path + 1];
            path_buffer[max_path] = '\0';

            PLATFORM.read_save_data(path_buffer,
                                    max_path,
                                    fs_contents_offset() + file * block_size +
                                        sizeof(FileContents::Header));

            callback(path_buffer);
        }

        offset += sizeof info;
    }
}



} // namespace ram_filesystem
