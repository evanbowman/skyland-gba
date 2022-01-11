#pragma once

#include "number/endian.hpp"
#include "number/int.h"
#include "platform/platform.hpp"
#include "vector.hpp"
#include <utility>



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



bool file_exists(Platform& pfrm, const char* path);



void destroy(Platform& pfrm);



size_t read_file_data(Platform& pfrm, const char* path, Vector<char>& output);



bool store_file_data(Platform& pfrm, const char* path, Vector<char>& data);



inline bool
store_file_data(Platform& pfrm, const char* path, const char* ptr, u32 length)
{
    Vector<char> buffer(pfrm);
    for (u32 i = 0; i < length; ++i) {
        buffer.push_back(ptr[i]);
    }
    buffer.push_back('\0');

    return store_file_data(pfrm, path, buffer);
}



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
        // NOTE: therefore, max filesystem size is 200 * 65535
        host_u16 next_; // Zero if no more blocks
        u8 checksum_;   // To check for file corruption
    } header_;

    static constexpr const auto capacity = block_size - sizeof header_;

    char data_[capacity];
};
static_assert(sizeof(FileContents) == block_size);


static_assert(max_path < FileContents::capacity);



Root load_root(Platform& pfrm);



template <typename F> void walk(Platform& pfrm, F&& callback)
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
                                fs_contents_offset() + file * block_size +
                                    sizeof(FileContents::Header));

            callback(path_buffer);
        }

        offset += sizeof info;
    }
}



inline void import_file_from_rom(Platform& pfrm,
                                 const char* dest_path,
                                 const char* src_path)
{
    if (auto data = pfrm.load_file_contents("scripts", src_path)) {
        Vector<char> vec(pfrm);
        while (*data not_eq '\0') {
            vec.push_back(*data);
        }
        vec.push_back('\0');
        store_file_data(pfrm, dest_path, vec);
    }
}



inline void import_file_from_rom_if_not_exists(Platform& pfrm,
                                               const char* dest_path,
                                               const char* src_path)
{
    if (not file_exists(pfrm, dest_path)) {
        import_file_from_rom(pfrm, dest_path, src_path);
    }
}



} // namespace ram_filesystem
