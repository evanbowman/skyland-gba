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


#include "ram_filesystem.hpp"
#include "bloomFilter.hpp"



namespace ram_filesystem
{



static BloomFilter<512> file_present_filter;



void __path_cache_insert(const char* path)
{
    file_present_filter.insert(path, str_len(path));
}



void __path_cache_create(Platform& pfrm)
{
    file_present_filter.clear();

    walk(pfrm, [&](const char* path) { __path_cache_insert(path); });
}



void __path_cache_destroy()
{
    file_present_filter.clear();
}



bool __path_cache_file_exists(const char* file_name)
{
    return file_present_filter.exists(file_name, str_len(file_name));
}



int fs_begin_offset;



static bool mounted = false;



bool is_mounted()
{
    return mounted;
}



int fs_offset()
{
    return fs_begin_offset;
}



int fs_contents_offset()
{
    // Block zero reserved for file root and fileinfo stuff.
    return fs_offset() + block_size;
}



Root load_root(Platform& pfrm)
{
    Root root;
    pfrm.read_save_data(&root, sizeof root, fs_offset());


    return root;
}



Statistics statistics(Platform& pfrm)
{
    const auto block_count = (pfrm.save_capacity() - fs_offset()) / block_size;

    Statistics stats;
    stats.blocks_available_ = 0;

    auto root = load_root(pfrm);
    auto freelist = root.freelist_.get();

    while (freelist) {
        FileContents::Header header;
        pfrm.read_save_data(&header,
                            sizeof header,
                            fs_contents_offset() + freelist * block_size);

        ++stats.blocks_available_;

        freelist = header.next_.get();
    }

    stats.blocks_used_ = block_count - stats.blocks_available_;

    return stats;
}



bool store_root(Platform& pfrm, const Root& root)
{
    return pfrm.write_save_data(&root, sizeof root, fs_offset());
}



void destroy(Platform& pfrm)
{
    char buffer[block_size * 2];
    __builtin_memset(buffer, 0, sizeof buffer);

    __path_cache_destroy();

    pfrm.write_save_data(buffer, sizeof buffer, fs_offset());

    mounted = false;
}



InitStatus initialize(Platform& pfrm, int fs_begin_offset)
{
    ram_filesystem::fs_begin_offset = fs_begin_offset;

    auto root = load_root(pfrm);

    pfrm.read_save_data(&root, sizeof root, fs_offset());

    static const char fs_version = '1';

    if (root.magic_[0] == '_' and root.magic_[1] == 'F' and
        root.magic_[2] == 'S' and root.magic_[3] == fs_version) {

        ::ram_filesystem::mounted = true;

        __path_cache_create(pfrm);

        // Already initialized previously.
        return InitStatus::already_initialized;
    }

    root.magic_[0] = '_';
    root.magic_[1] = 'F';
    root.magic_[2] = 'S';
    root.magic_[3] = fs_version;

    root.file_count_.set(0);
    root.freelist_.set(0);

    // Construct the freelist. Only needs to be done once, the first time that
    // the game boots, as the filesystem persists in SRAM.
    const auto block_count = (pfrm.save_capacity() - fs_offset()) / block_size;
    auto offset = fs_contents_offset();
    for (int i = 1; i < block_count; ++i) {
        FileContents::Header header;
        pfrm.read_save_data(&header, sizeof header, offset);
        header.next_.set(root.freelist_.get());
        pfrm.write_save_data(&header, sizeof header, offset);

        root.freelist_.set(i - 1);

        offset += block_size;
    }

    if (store_root(pfrm, root)) {
        ::ram_filesystem::mounted = true;

        __path_cache_create(pfrm);

        return InitStatus::initialized;
    }

    return InitStatus::failed;
}



static u16 allocate_file_chunk(Platform& pfrm)
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



void link_file(Platform& pfrm, u16 file, u16 length)
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



void free_file(Platform& pfrm, u16 file)
{
    auto root = load_root(pfrm);

    while (file) {
        FileContents::Header header;
        pfrm.read_save_data(
            &header, sizeof header, fs_contents_offset() + file * block_size);

        auto freelist = root.freelist_.get();
        root.freelist_.set(file);
        auto next_file = header.next_.get();
        header.next_.set(freelist);

        pfrm.write_save_data(
            &header, sizeof header, fs_contents_offset() + file * block_size);

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
                                fs_contents_offset() + file * block_size +
                                    sizeof(FileContents::Header));

            if (str_cmp(path_buffer, path) == 0) {
                callback(info, file, offset);
                return;
            }
        }

        offset += sizeof info;
    }
}



bool file_exists(Platform& pfrm, const char* path)
{
    if (not ::ram_filesystem::mounted) {
        return false;
    }


    if (__path_cache_file_exists(path)) {
        bool found = false;

        with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {
            found = true;
        });

        return found;

    } else {

        return false;
    }
}



void unlink_file(Platform& pfrm, const char* path)
{
    if (not ::ram_filesystem::mounted) {
        return;
    }

    if (not __path_cache_file_exists(path)) {
        return;
    }

    bool freed_file = false;

    with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {
        // Unbind the existing file
        info.file_size_.set(0);
        info.file_contents_.set(0);
        pfrm.write_save_data(&info, sizeof info, fs_offset);

        freed_file = true;

        free_file(pfrm, file);
    });

    if (freed_file) {
        __path_cache_destroy();
        __path_cache_create(pfrm);
    }
}



static const u16 crctable[256] = {
    0x0000, 0x1189, 0x2312, 0x329B, 0x4624, 0x57AD, 0x6536, 0x74BF, 0x8C48,
    0x9DC1, 0xAF5A, 0xBED3, 0xCA6C, 0xDBE5, 0xE97E, 0xF8F7, 0x0919, 0x1890,
    0x2A0B, 0x3B82, 0x4F3D, 0x5EB4, 0x6C2F, 0x7DA6, 0x8551, 0x94D8, 0xA643,
    0xB7CA, 0xC375, 0xD2FC, 0xE067, 0xF1EE, 0x1232, 0x03BB, 0x3120, 0x20A9,
    0x5416, 0x459F, 0x7704, 0x668D, 0x9E7A, 0x8FF3, 0xBD68, 0xACE1, 0xD85E,
    0xC9D7, 0xFB4C, 0xEAC5, 0x1B2B, 0x0AA2, 0x3839, 0x29B0, 0x5D0F, 0x4C86,
    0x7E1D, 0x6F94, 0x9763, 0x86EA, 0xB471, 0xA5F8, 0xD147, 0xC0CE, 0xF255,
    0xE3DC, 0x2464, 0x35ED, 0x0776, 0x16FF, 0x6240, 0x73C9, 0x4152, 0x50DB,
    0xA82C, 0xB9A5, 0x8B3E, 0x9AB7, 0xEE08, 0xFF81, 0xCD1A, 0xDC93, 0x2D7D,
    0x3CF4, 0x0E6F, 0x1FE6, 0x6B59, 0x7AD0, 0x484B, 0x59C2, 0xA135, 0xB0BC,
    0x8227, 0x93AE, 0xE711, 0xF698, 0xC403, 0xD58A, 0x3656, 0x27DF, 0x1544,
    0x04CD, 0x7072, 0x61FB, 0x5360, 0x42E9, 0xBA1E, 0xAB97, 0x990C, 0x8885,
    0xFC3A, 0xEDB3, 0xDF28, 0xCEA1, 0x3F4F, 0x2EC6, 0x1C5D, 0x0DD4, 0x796B,
    0x68E2, 0x5A79, 0x4BF0, 0xB307, 0xA28E, 0x9015, 0x819C, 0xF523, 0xE4AA,
    0xD631, 0xC7B8, 0x48C8, 0x5941, 0x6BDA, 0x7A53, 0x0EEC, 0x1F65, 0x2DFE,
    0x3C77, 0xC480, 0xD509, 0xE792, 0xF61B, 0x82A4, 0x932D, 0xA1B6, 0xB03F,
    0x41D1, 0x5058, 0x62C3, 0x734A, 0x07F5, 0x167C, 0x24E7, 0x356E, 0xCD99,
    0xDC10, 0xEE8B, 0xFF02, 0x8BBD, 0x9A34, 0xA8AF, 0xB926, 0x5AFA, 0x4B73,
    0x79E8, 0x6861, 0x1CDE, 0x0D57, 0x3FCC, 0x2E45, 0xD6B2, 0xC73B, 0xF5A0,
    0xE429, 0x9096, 0x811F, 0xB384, 0xA20D, 0x53E3, 0x426A, 0x70F1, 0x6178,
    0x15C7, 0x044E, 0x36D5, 0x275C, 0xDFAB, 0xCE22, 0xFCB9, 0xED30, 0x998F,
    0x8806, 0xBA9D, 0xAB14, 0x6CAC, 0x7D25, 0x4FBE, 0x5E37, 0x2A88, 0x3B01,
    0x099A, 0x1813, 0xE0E4, 0xF16D, 0xC3F6, 0xD27F, 0xA6C0, 0xB749, 0x85D2,
    0x945B, 0x65B5, 0x743C, 0x46A7, 0x572E, 0x2391, 0x3218, 0x0083, 0x110A,
    0xE9FD, 0xF874, 0xCAEF, 0xDB66, 0xAFD9, 0xBE50, 0x8CCB, 0x9D42, 0x7E9E,
    0x6F17, 0x5D8C, 0x4C05, 0x38BA, 0x2933, 0x1BA8, 0x0A21, 0xF2D6, 0xE35F,
    0xD1C4, 0xC04D, 0xB4F2, 0xA57B, 0x97E0, 0x8669, 0x7787, 0x660E, 0x5495,
    0x451C, 0x31A3, 0x202A, 0x12B1, 0x0338, 0xFBCF, 0xEA46, 0xD8DD, 0xC954,
    0xBDEB, 0xAC62, 0x9EF9, 0x8F70};



static u16 crc16(u16 crc, const void* c_ptr, size_t len)
{
    const u8* c = (const u8*)c_ptr;

    while (len--) {
        crc = (crc << 8) ^ crctable[((crc >> 8) ^ *c++)];
    }

    return crc;
}



static u16 checksum(const FileContents& contents)
{
    return crc16(0xFFFF, contents.data_, FileContents::capacity);
}



size_t read_file_data(Platform& pfrm, const char* path, Vector<char>& output)
{
    if (not ::ram_filesystem::mounted) {
        return 0;
    }

    if (not __path_cache_file_exists(path)) {
        return 0;
    }

    const auto path_len = str_len(path);

    with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {
        FileContents contents;

        pfrm.read_save_data(&contents,
                            sizeof contents,
                            fs_contents_offset() + file * block_size);

        auto on_corruption = [&] {
            output.clear();
            const char* msg = "DISK_CORRUPTION";
            const auto msg_len = str_len(msg);
            for (u32 i = 0; i < msg_len; ++i) {
                output.push_back(msg[i]);
            }
        };

        if (checksum(contents) not_eq contents.header_.checksum_.get()) {
            on_corruption();
            return;
        }

        const auto len = info.file_size_.get() - (path_len + 1);

        for (u16 i = path_len + 1;
             i < FileContents::capacity and output.size() not_eq len;
             ++i) {

            output.push_back(contents.data_[i]);
        }

        file = contents.header_.next_.get();

        while (file) {
            pfrm.read_save_data(&contents,
                                sizeof contents,
                                fs_contents_offset() + file * block_size);

            if (checksum(contents) not_eq contents.header_.checksum_.get()) {
                on_corruption();
                return;
            }

            for (u16 i = 0;
                 i < FileContents::capacity and output.size() not_eq len;
                 ++i) {

                output.push_back(contents.data_[i]);
            }

            file = contents.header_.next_.get();
        }
    });

    auto result = output.size();

    output.push_back('\0');

    return result;
}



bool store_file_data(Platform& pfrm, const char* path, Vector<char>& data)
{
    if (not ::ram_filesystem::mounted) {
        return false;
    }

    unlink_file(pfrm, path);

    const u16 path_len = str_len(path);

    if (path_len > max_path) {
        return false;
    }

    const int length = data.size() - 1; // -1 for the assumed null terminator.
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
        std::min((int)FileContents::capacity - (path_len + 1), (int)length);


    for (int i = 0; i < initial_data_copy; ++i) {
        contents.data_[i + path_len + 1] = data[i];
    }

    int offset = initial_data_copy;

    remaining -= initial_data_copy;

    auto store_chunk = [&] {
        contents.header_.checksum_.set(checksum(contents));
        pfrm.write_save_data((u8*)&contents,
                             sizeof contents,
                             fs_contents_offset() + file * block_size);
    };

    if (remaining == 0) {
        store_chunk();
        __path_cache_insert(path);
        link_file(pfrm, file_begin, length + path_len + 1);
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

        const auto copy_len =
            std::min((int)FileContents::capacity, (int)remaining);

        for (int i = 0; i < copy_len; ++i) {
            contents.data_[i] = data[offset + i];
        }

        offset += copy_len;
        remaining -= copy_len;
    }

    store_chunk();

    __path_cache_insert(path);
    link_file(pfrm, file_begin, length + path_len + 1);

    return true;
}



} // namespace ram_filesystem
