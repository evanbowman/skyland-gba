#pragma once

#include "number/int.h"
#include "number/endian.hpp"
#include <utility>
#include "platform/platform.hpp"



// A filesystem for Save RAM.
// Platform::read_save_data() and Platform::write_save_data() provide an
// abstraction for dealing with save memory. Technically, there's nothing
// platform-specific in this filesystem implementation, although desktop
// PCs have actual filesystems, so there's no reason to use this code for a
// hosted environment.



namespace ram_filesystem {



enum {
    // Maximum supported path. Technically, the filesystem supports paths as
    // large as the block size, but that'd be super wasteful. 64 bytes should be
    // plenty.
    max_path = 64,

    // Block size used for the filesystem. Files will be sliced into ~200 byte
    // blocks. A larger block size will save a few bytes, as each byte stores a
    // two byte index representing the location of the next block in the file. A
    // smaller block size often ultimately saves more space, as a large block
    // size does not efficiently store small files.
    block_size = 200,

    ram_size = 32000,
};



struct Statistics {
    u16 blocks_used_;
    u16 blocks_available_;
};



Statistics statistics(Platform& pfrm);



void initialize(Platform& pfrm, int fs_begin_offset);



void link_file(Platform& pfrm, u16 file, u16 length);



void free_file(Platform& pfrm, u16 file);



void unlink_file(Platform& pfrm, const char* path);



size_t read_file_data(Platform& pfrm,
                      const char* path,
                      ScratchBufferPtr output_buffer);



bool store_file_data(Platform& pfrm,
                     const char* path,
                     const char* data,
                     const s16 length);



int fs_offset();



int fs_contents_offset();




struct Root {
    char magic_[4];
    host_u16 file_count_;
    host_u16 freelist_;
    // FileInfo file_info_[file_count_];
};



struct FileInfo {
    host_u16 file_size_;
    host_u16 file_contents_;
};



struct FileContents {
    struct Header {
        host_u16 next_; // zero if no more blocks
    } header_;

    static constexpr const auto capacity = block_size - sizeof header_;

    char data_[capacity];
}; static_assert(sizeof(FileContents) == block_size);


static_assert(max_path < FileContents::capacity);



Root load_root(Platform& pfrm);



template <typename F>
void walk(Platform& pfrm, F&& callback)
{
    auto root = load_root(pfrm);

    auto offset = fs_offset() + sizeof(Root);

    for (int i = 0; i < root.file_count_.get(); ++i) {
        FileInfo info;
        pfrm.read_save_data(&info, sizeof info, offset);

        if (auto file = info.file_contents_.get()) {
            char path_buffer[max_path + 1];
            path_buffer[max_path] = '\0';

            pfrm.read_save_data(path_buffer,
                                max_path,
                                fs_contents_offset() + file * block_size
                                + sizeof(FileContents::Header));

            callback(path_buffer);
        }

        offset += sizeof info;
    }
}



} // ram_filesystem
