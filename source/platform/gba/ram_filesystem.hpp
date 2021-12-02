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


static_assert(max_path < FileContents::capacity);


constexpr inline int fs_offset()
{
    return 2000;
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


inline void link_file(Platform& pfrm, u16 file, u16 length)
{
    auto root = load_root(pfrm);

    auto offset = fs_offset() + sizeof(Root);

    for (int i = 0; i < root.file_count_.get(); ++i) {
        FileInfo info;
        pfrm.read_save_data(&info, sizeof info, offset);

        if (info.file_contents_.get() == 0 and info.file_size_.get() == 0) {
            info.file_size_.set(length);
            info.file_contents_.set(file);
            pfrm.write_save_data(&info, sizeof info, offset);
            return;
        }

        offset += sizeof info;
    }

    FileInfo info;
    info.file_size_.set(length);
    info.file_contents_.set(file);
    pfrm.write_save_data(&info, sizeof info, offset);

    root.file_count_.set(root.file_count_.get() + 1);

    store_root(pfrm, root);
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


template <typename F>
void with_file(Platform& pfrm, const char* path, F&& callback)
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

            if (str_cmp(path_buffer, path) == 0) {
                callback(info, file, offset);
                return;
            }
        }

        offset += sizeof info;
    }
}


inline void unlink_file(Platform& pfrm, const char* path)
{
    with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {
        // Unbind the existing file
        info.file_size_.set(0);
        info.file_contents_.set(0);
        pfrm.write_save_data(&info, sizeof info, fs_offset);

        free_file(pfrm, file);
    });
}


inline size_t read_file_data(Platform& pfrm,
                             const char* path,
                             ScratchBufferPtr output_buffer)
{
    const auto path_len = str_len(path);

    __builtin_memset(output_buffer->data_,
                     0,
                     sizeof output_buffer->data_);


    int write_pos = 0;

    with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {

        FileContents contents;

        pfrm.read_save_data(&contents, sizeof contents,
                            fs_contents_offset() + file * block_size);

        for (u16 i = path_len + 1; i < FileContents::capacity; ++i) {
            output_buffer->data_[write_pos++] = contents.data_[i];
        }

        file = contents.header_.next_.get();

        while (file) {
            pfrm.read_save_data(&contents, sizeof contents,
                                fs_contents_offset() + file * block_size);

            for (u16 i = 0; i < FileContents::capacity; ++i) {
                if (write_pos == SCRATCH_BUFFER_SIZE) {
                    write_pos = 0;
                    return;
                }
                output_buffer->data_[write_pos++] = contents.data_[i];
            }

            file = contents.header_.next_.get();
        }
    });

    if (write_pos == 0) {
        pfrm.fatal("here");
    }

    return write_pos;
}


inline bool store_file_data(Platform& pfrm,
                            const char* path,
                            const char* data,
                            const s16 length)
{
    unlink_file(pfrm, path);

    const u16 path_len = str_len(path);

    if (path_len > max_path) {
        return false;
    }

    u16 remaining = length + path_len + 1;

    const auto file_begin = allocate_file_chunk(pfrm);
    auto file = file_begin;
    if (file == 0) {
        return false;
    }

    FileContents contents;
    __builtin_memset(&contents, 0, sizeof contents);

    memcpy(contents.data_, path, path_len + 1);
    remaining -= path_len + 1;

    const auto initial_data_copy =
        std::min((int)FileContents::capacity - (path_len + 1),
                 (int)length);

    memcpy(contents.data_ + (path_len + 1),
           data,
           initial_data_copy);

    data += initial_data_copy;
    remaining -= initial_data_copy;

    auto store_chunk = [&] {
        pfrm.write_save_data((u8*)&contents,
                             sizeof contents,
                             fs_contents_offset() + file * block_size);
    };

    if (remaining == 0) {
        store_chunk();
        return true;
    }

    while (remaining) {
        auto next_file = allocate_file_chunk(pfrm);
        if (next_file == 0) {
            free_file(pfrm, file_begin);
            return false;
        }
        contents.header_.next_.set(next_file);

        store_chunk();

        __builtin_memset(&contents, 0, sizeof contents);

        file = next_file;

        const auto copy_len = std::min((int)FileContents::capacity,
                                       (int)remaining);

        memcpy(contents.data_,
               data,
               copy_len);

        data += copy_len;
        remaining -= copy_len;
    }

    store_chunk();


    link_file(pfrm, file_begin, length + path_len + 1);


    return true;
}


} // ram_filesystem
