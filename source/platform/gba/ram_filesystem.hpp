#pragma once

#include "number/int.h"
#include "number/endian.hpp"
#include <utility>
#include "platform/platform.hpp"


namespace ram_filesystem {


struct Root {
    char magic_[4];
    host_u16 file_count_;
};


struct FileHeader {
    host_u16 size_;
    // char* name_; (null terminated)
};


enum {
    max_path = 32
};


inline int fs_offset()
{
    return 2000;
}


inline void* fs_begin()
{
    return (u8*)0x0E000000 + fs_offset();
}


inline Root load_root(Platform& pfrm)
{
    Root root;
    pfrm.read_save_data(&root, sizeof root, fs_offset());

    return root;
}


inline void store_root(Platform& pfrm, const Root& root)
{
    pfrm.write_save_data(&root, sizeof root, fs_offset());
}


inline void initialize(Platform& pfrm)
{
    auto root = load_root(pfrm);

    pfrm.read_save_data(&root, sizeof root, fs_offset());

    if (root.magic_[0] == '_' and
        root.magic_[1] == 'F' and
        root.magic_[2] == 'S' and
        root.magic_[3] == '_') {

        // Already initialized previously.
        return;
    }

    root.magic_[0] = '_';
    root.magic_[1] = 'F';
    root.magic_[2] = 'S';
    root.magic_[3] = '_';

    root.file_count_.set(0);

    store_root(pfrm, root);
}


std::pair<const char*, u16> load_file(const char* path);


bool store_file_data(Platform& pfrm,
                     const char* path,
                     const char* data,
                     u16 length)
{
    const auto path_len = str_len(path);
    if (path_len > max_path) {
        return false;
    }


    auto root = load_root(pfrm);

    u16 offset = fs_offset() + sizeof(Root);


    for (int i = 0; i < root.file_count_.get(); ++i) {
        FileHeader header;
        pfrm.read_save_data(&header, sizeof header, offset);
        offset += sizeof header;

        char path_buffer[max_path + 1];
        pfrm.read_save_data(path_buffer, max_path + 1, offset);
        if (str_cmp(path, path_buffer) == 0) {
            // File already exists. If the file shrank, we do not need to
            // move anything around.
        }

        offset += header.size_.get();
    }


    FileHeader header;
    header.size_.set(length + path_len + 1);
    pfrm.write_save_data(&header, sizeof header, offset);
    offset += sizeof header;

    pfrm.write_save_data(path, path_len + 1, offset);
    offset += path_len + 1; // +1 for null terminator in path string.

    pfrm.write_save_data(data, length, offset);

    // Because we created a new file.
    root.file_count_.set(root.file_count_.get() + 1);
    store_root(pfrm, root);

    return true;
}


} // ram_filesystem
