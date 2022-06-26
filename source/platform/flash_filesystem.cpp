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


#include "flash_filesystem.hpp"
#include "string.hpp"
#include "bloomFilter.hpp"



#ifndef __GBA__
class Platform
{

public:


    ~Platform()
    {
        std::ofstream stream("Skyland.logstructured.edit.sav",
                             std::ios::out | std::ios::binary);

        for (u8 byte : data_) {
            stream.write((const char*)&byte, 1);
        }
    }


    Platform()
    {
        std::ifstream stream("Skyland.logstructured.sav",
                             std::ios::in | std::ios::binary);
        std::vector<uint8_t> contents((std::istreambuf_iterator<char>(stream)),
                                      std::istreambuf_iterator<char>());
        data_ = contents;

        std::cout << "loaded data, size: " << data_.size() << std::endl;
    }


    void erase_save_sector()
    {
        auto cap = save_capacity();
        data_.clear();
        for (int i = 0; i < cap; ++i) {
            data_.push_back(0xff);
        }
    }


    bool write_save_data(const void* data, u32 data_length, u32 offset)
    {
        for (u32 i = 0; i < data_length; ++i) {
            data_[offset + i] = ((u8*)data)[i];
        }
        return true;
    }


    bool read_save_data(void* buffer, u32 data_length, u32 offset)
    {
        for (u32 i = 0; i < data_length; ++i) {
            ((u8*)buffer)[i] = data_[offset + i];
        }
        return true;
    }


    int save_capacity()
    {
        return data_.size();
    }


private:
    std::vector<uint8_t> data_;
};

#else
#include "platform/platform.hpp"
#endif // __GBA__



////////////////////////////////////////////////////////////////////////////////


// PREAMBLE: I used to use a way more efficient journaled filesystem. But then I
// needed to support flash chips. So long to my beautiful filesystem, I'm using
// this flash implementation instead. Basically, flash chips support a limited
// number of write/erase cycles. After an erase cycle, bytes may only be written
// once, after which, a byte may not be written to the same address without
// erasing a whole 64kb sector of the cartridge rom. This log-structured
// filesystem always writes new data to the end of the filesystem memory, except
// for slots in the previous entries used for invalidating old versions of a
// file. When the fs runs out of space, it erases the whole save media and
// rewrites the data, compacted to remove gaps.
//
// NOTE: we don't actually optimize flash writes in the platform implementation
// yet, and this filesystem library may require some more changes. But the
// structure of the filesystem, perhaps with some slight edits, is more
// appropriate for optimizing flash writes than the previous implementation.



namespace flash_filesystem
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



bool __path_cache_file_exists_maybe(const char* file_name)
{
    return file_present_filter.exists(file_name, str_len(file_name));
}



struct Root
{
    static constexpr const char* magic_val = "_FS3_LOG";

    u8 magic_[8];
};



struct Record
{
    // NOTE: u16 because our flash chip writes in halfwords.
    enum InvalidateStatus : u16
    {
        // A flash erase will set all values in a sector to 0xffff. This is
        // simply how flash storage works. A flash write can only change bits
        // from one to zero, an erase operation sets all bits in a flash sector
        // to one.
        valid = 0xffff,
        invalid = 0x0000
    };

    HostInteger<u16> invalidate_;

    struct FileInfo
    {
        // Sanity check byte. In case a cosmic ray flipped a bit or something.
        u8 crc_;
        u8 flags_; // unused
        u8 name_length_;
        host_u16 data_length_;


        // NOTE: appended data:
        //
        // char name_[name_length_];
        // char data_[data_length_];
    } file_info_;


    u32 appended_size() const
    {
        return file_info_.name_length_ + file_info_.data_length_.get();
    }


    u32 full_size() const
    {
        return sizeof(Record) + appended_size();
    }

};



static u32 start_offset = 0;
static u32 end_offset = 0;
static u32 gap_space = 0;



void destroy(Platform& pfrm)
{
    pfrm.erase_save_sector();
}



u32 sector_used()
{
    return end_offset - start_offset;
}



u32 sector_avail(Platform& pfrm)
{
    return pfrm.save_capacity() - end_offset;
}



Statistics statistics(Platform& pfrm)
{
    Statistics ret;
    ret.blocks_used_ = sector_used() / 200 - gap_space / 200;
    ret.blocks_available_ = sector_avail(pfrm) / 200 + gap_space / 200;

    return ret;
}



Root load_root(Platform& pfrm)
{
    Root root;
    pfrm.read_save_data(&root, sizeof root, start_offset);

    return root;
}



static void init_root(Platform& pfrm, Root& root)
{
    memcpy(root.magic_, Root::magic_val, 8);
    pfrm.write_save_data(&root, sizeof root, start_offset);
}



InitStatus initialize(Platform& pfrm, u32 offset)
{
    start_offset = offset;
    auto root = load_root(pfrm);

    if (memcmp(root.magic_, Root::magic_val, 8) not_eq 0) {
        pfrm.erase_save_sector();

        init_root(pfrm, root);

        end_offset = start_offset;

        __path_cache_create(pfrm);

        return initialized;
    }

    info(pfrm, "flash fs found root...");

    offset += sizeof(Root);

    if (offset not_eq 16) {
        while (true) ;
    }

    while (true) {
        Record r;
        pfrm.read_save_data(&r, sizeof r, offset);

        if (r.file_info_.name_length_ == 0xff) {
            // uninitialized, as it holds the default flash erase value.
            break;
        }

        info(pfrm, "flash fs got record");

        if (r.invalidate_.get() not_eq Record::InvalidateStatus::valid) {
            gap_space += r.full_size();
        }

        offset += r.full_size();
    }
    end_offset = offset;

    __path_cache_create(pfrm);

    return already_initialized;
}



void walk(Platform& pfrm, Function<32, void(const char*)> callback)
{
    auto offset = start_offset;

    offset += sizeof(Root);

    while (true) {
        Record r;
        pfrm.read_save_data(&r, sizeof r, offset);

        if (r.file_info_.name_length_ == 0xff) {
            // uninitialized, as it holds the default flash erase value.
            break;
        }

        offset += sizeof r;

        char file_name[FS_MAX_PATH + 1];
        memset(file_name, 0, FS_MAX_PATH + 1);

        pfrm.read_save_data(&file_name, r.file_info_.name_length_, offset);


        if (r.invalidate_.get() == Record::InvalidateStatus::valid) {
            callback(file_name);
        } else {
#ifndef __GBA__
            callback(("(INVALID)" + std::string(file_name)).c_str());
#endif
        }

        offset += r.appended_size();
    }
}



int find_file(Platform& pfrm, const char* path, Record& result)
{
    auto offset = start_offset;
    offset += sizeof(Root);

    while (true) {
        Record r;
        const auto record_offset = offset;

        pfrm.read_save_data(&r, sizeof r, offset);

        if (r.file_info_.name_length_ == 0xff) {
            // uninitialized, as it holds the default flash erase value.
            // i.e. we're at the end of the filesystem storage. Nothing's been
            // written here. We're using the name_length value as an identity
            // test for file block presence, so we don't need to store another
            // field, but obviously this prevents us from storing a file with
            // name length == 255.
            break;
        }

        offset += sizeof r;

        char file_name[FS_MAX_PATH + 1];
        memset(file_name, 0, FS_MAX_PATH + 1);

        pfrm.read_save_data(&file_name, r.file_info_.name_length_, offset);

        if (r.invalidate_.get() == Record::InvalidateStatus::valid and
            str_eq(path, file_name)) {
            result = r;
            return record_offset;
        }

        offset += r.appended_size();
    }

    return -1;
}



bool file_exists(Platform& pfrm, const char* path)
{
    if (not __path_cache_file_exists_maybe(path)) {
        return false;
    }

    Record r;
    return find_file(pfrm, path, r) not_eq -1;
}



void unlink_file(Platform& pfrm, const char* path)
{
    if (not __path_cache_file_exists_maybe(path)) {
        return;
    }

    Record r;

    bool freed = false;

    auto off = find_file(pfrm, path, r);
    while (off not_eq -1) {
        // NOTE: first byte of record holds invalidate bytes.
        static_assert(sizeof(Record) == sizeof(Record::FileInfo) + 2);
        auto stat = Record::InvalidateStatus::invalid;
        pfrm.write_save_data(&stat, 2, off);

        gap_space += r.full_size();
        freed = true;

        off = find_file(pfrm, path, r);
    }

    if (freed) {
        __path_cache_destroy();
        __path_cache_create(pfrm);
    }
}



// Ok, now we need to copy every non-dead chunk in the filesystem into ram,
// erase the flash sector, and write it back...
static void compact(Platform& pfrm)
{
    info(pfrm, "flash fs start compaction...");

    Vector<char> data;

    // FIXME: do not hard-code this size. file breaks should be some sort of
    // list?
    Buffer<u32, 100> breaks;

    auto offset = start_offset;
    offset += sizeof(Root);

    while (true) {
        Record r;
        pfrm.read_save_data(&r, sizeof r, offset);

        if (r.file_info_.name_length_ == 0xff) {
            // uninitialized, as it holds the default flash erase value.
            break;
        }

        offset += sizeof r;

        char file_name[FS_MAX_PATH + 1];
        memset(file_name, 0, FS_MAX_PATH + 1);

        pfrm.read_save_data(&file_name, r.file_info_.name_length_, offset);

        if (r.invalidate_.get() == Record::InvalidateStatus::valid) {

            r.invalidate_.set(Record::InvalidateStatus::invalid);
            static_assert(sizeof(Record) == sizeof(Record::FileInfo) + 2);
            // NOTE: we don't want to ever write the first byte in the record,
            // as we use this byte for invalidating entries. Keep track of the
            // positions to avoid writing back after the erase operation.
            breaks.push_back(data.size());
            for (u32 i = 0; i < sizeof r; ++i) {
                data.push_back(((u8*)&r)[i]);
            }
            for (int i = 0; i < r.file_info_.name_length_; ++i) {
                data.push_back(file_name[i]);
            }
            offset += r.file_info_.name_length_;
            for (u32 i = 0; i < r.file_info_.data_length_.get(); ++i) {
                u8 val;
                pfrm.read_save_data(&val, 1, offset++);
                data.push_back(val);
            }
        } else {
            offset += r.appended_size();
        }
    }

    pfrm.erase_save_sector();

    const auto start_align = start_offset + sizeof(Root);
    for (u32 i = 0; i < data.size(); ++i) {
        if (i == breaks[0]) {
            breaks.erase(breaks.begin());
            ++i; // Skip the next byte too.
            static_assert(sizeof(Record) == sizeof(Record::FileInfo) + 2);
        } else {
            auto val = data[i];
            pfrm.write_save_data(&val, 1, start_align + i);
        }
    }

    end_offset = start_align + data.size();
    gap_space = 0;

    Root root;
    init_root(pfrm, root);

    info(pfrm, "flash fs completed compaction!");
}



bool store_file_data(Platform& pfrm, const char* path, Vector<char>& data)
{
    // Append a new file to the end of the filesystem log.

    auto path_len = str_len(path);

    u32 required_space = data.size() + path_len + sizeof(Record);

    const auto freed_space = [&] {
        if (not __path_cache_file_exists_maybe(path)) {
            return u32(0);
        }
        Record found;
        auto result = find_file(pfrm, path, found);
        if (result not_eq -1) {
            return found.full_size();
        }
        return u32(0);
    }();

    auto avail_space = (freed_space + sector_avail(pfrm)) - sizeof(Record);

    if ((required_space >= avail_space) and
        avail_space + gap_space > required_space) {
        // We can reclaim enough space to store the file by compacting the
        // storage data to squeeze out gaps.
        unlink_file(pfrm, path);
        compact(pfrm);
    } else if (required_space >= avail_space) {
        // NOTE: don't unlink the existing file, we don't have enough space to
        // store the replacement.
        return false;
    } else {
        // We have enough space, but, of course, we do want to delete the
        // previous version of the file (if it exists).
        unlink_file(pfrm, path);
    }

    auto off = end_offset;
    static_assert(sizeof(Record) == sizeof(Record::FileInfo) + 2);
    ++off; // Skip the invalid bytes, which we don't want to write yet.
    ++off;


    Record::FileInfo info;
    info.name_length_ = path_len;
    info.data_length_.set(data.size());

    pfrm.write_save_data(&info, sizeof info, off);
    off += sizeof info;

    pfrm.write_save_data(path, path_len, off);
    off += path_len;


    for (auto it = data.begin(); it not_eq data.end(); ++it) {
        auto val = *it;
        pfrm.write_save_data(&val, 1, off);
        ++off;
    }

    end_offset = off;

    __path_cache_insert(path);

    return true;
}



u32 read_file_data(Platform& pfrm, const char* path, Vector<char>& output)
{
    if (not __path_cache_file_exists_maybe(path)) {
        return 0;
    }

    Record r;

    auto offset = find_file(pfrm, path, r);
    if (offset == -1) {
        return 0;
    }

    offset += sizeof r;
    offset += r.file_info_.name_length_;

    for (int i = 0; i < r.file_info_.data_length_.get(); ++i) {
        char val;
        pfrm.read_save_data(&val, 1, offset++);
        output.push_back(val);
    }

    return output.size();
}



}



#ifndef __GBA__
int main()
{
    Platform pfrm;

    flash_filesystem::initialize(pfrm, 8);

    flash_filesystem::walk(pfrm, [](const char* name) {
                                     std::cout << name << std::endl;
                                 });

    std::cout << "used: " << flash_filesystem::sector_used() << std::endl;
    std::cout << "avail: " << flash_filesystem::sector_avail(pfrm) << std::endl;

    Vector<char> test_data;
    flash_filesystem::read_file_data(pfrm, "/mods/init.lisp", test_data);
    std::string str;
    for (char c : test_data) {
        str.push_back(c);
    }
    std::cout << "got file: \n" << str << std::endl;


    puts("test...");
    Vector<char> test;
    for (int i = 0; i < 1000; ++i) {
        test.push_back('a');
    }

    flash_filesystem::store_file_data(pfrm, "/test.dat", test);
    flash_filesystem::store_file_data(pfrm, "/mods/init.lisp", test);

    puts("");

    flash_filesystem::walk(pfrm, [](const char* name) {
                                     std::cout << name << std::endl;
                                 });

    std::cout << "used: " << flash_filesystem::sector_used() << std::endl;
    std::cout << "avail: " << flash_filesystem::sector_avail(pfrm) << std::endl;
    std::cout << "gap: " << flash_filesystem::gap_space << std::endl;



    puts("compact!");
    flash_filesystem::compact(pfrm);
    puts("");

    flash_filesystem::walk(pfrm, [](const char* name) {
                                     std::cout << name << std::endl;
                                 });

    std::cout << "used: " << flash_filesystem::sector_used() << std::endl;
    std::cout << "avail: " << flash_filesystem::sector_avail(pfrm) << std::endl;
    std::cout << "gap: " << flash_filesystem::gap_space << std::endl;

}
#endif // __GBA__
