#pragma once

#include "number/int.h"
#include "number/endian.hpp"
#include <utility>
#include "platform/platform.hpp"


// A filesystem for SRAM.


namespace ram_filesystem {


enum {
    max_path = 32,
    block_size = 200,
};


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


constexpr inline int fs_offset()
{
    return 2000;
}


inline void* fs_begin()
{
    return (u8*)0x0E000000 + fs_offset();
}


inline int fs_contents_offset()
{
    // Block zero reserved for file root and fileinfo stuff.
    return fs_offset() + block_size;
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
    root.freelist_.set(0);

    // Construct the freelist. Only needs to be done once, the first time that
    // the game boots, as the filesystem persists in SRAM.
    const auto block_count = (32000 - fs_offset()) / block_size;
    auto offset = fs_contents_offset();
    for (int i = 1; i < block_count; ++i) {
        FileContents::Header header;
        pfrm.read_save_data(&header, sizeof header, offset);
        header.next_.set(root.freelist_.get());
        pfrm.write_save_data(&header, sizeof header, offset);

        root.freelist_.set(i - 1);

        offset += block_size;
    }

    store_root(pfrm, root);
}


std::pair<const char*, u16> load_file(const char* path);


inline u16 allocate_file_chunk(Platform& pfrm)
{
    auto root = load_root(pfrm);

    if (root.freelist_.get() == 0) {
        return 0;
    } else {
        auto allocated = root.freelist_.get();

        FileContents::Header header;
        pfrm.read_save_data(&header,
                            sizeof header,
                            fs_contents_offset() + allocated * block_size);

        root.freelist_.set(header.next_.get());

        store_root(pfrm, root);

        return allocated;
    }
}


inline void free_file(Platform& pfrm, u16 file)
{
    auto root = load_root(pfrm);

    while (file) {
        FileContents::Header header;
        pfrm.read_save_data(&header,
                            sizeof header,
                            fs_contents_offset() + file * block_size);

        auto freelist = root.freelist_.get();
        root.freelist_.set(file);
        auto next_file = header.next_.get();
        header.next_.set(freelist);

        pfrm.write_save_data(&header,
                             sizeof header,
                             fs_contents_offset() + file * block_size);

        file = next_file;
    }

    store_root(pfrm, root);
}


inline bool store_file_data(Platform& pfrm,
                            const char* path,
                            const char* data,
                            const s16 length)
{
    const auto path_len = str_len(path);

    u16 tail = 0;

    u16 bytes_written = 0;

    while (bytes_written not_eq length) {

        auto file = allocate_file_chunk(pfrm);
        if (file == 0) {
            free_file(pfrm, tail);
            return false;
        }

        FileContents contents;
        __builtin_memset(&contents, 0, sizeof contents);

        const u16 remaining = length - bytes_written;
        if (remaining >= FileContents::capacity) {
            memcpy(contents.data_,
                   (data + length) - (bytes_written + FileContents::capacity),
                   FileContents::capacity);
            bytes_written += FileContents::capacity;
        } else {
            memcpy(contents.data_,
                   data,
                   remaining);
            bytes_written += remaining;
        }

        contents.header_.next_.set(tail);
        tail = file;

        pfrm.write_save_data((u8*)&contents,
                             sizeof contents,
                             fs_contents_offset() + file * block_size);
    }

    return true;
}


} // ram_filesystem
