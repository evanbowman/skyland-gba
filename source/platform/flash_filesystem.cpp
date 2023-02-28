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
#include "bloomFilter.hpp"
#include "compression.hpp"
#include "string.hpp"



#ifdef __TEST__


void arabic__to_string(int num, char* buffer, int base)
{
    int i = 0;
    bool is_negative = false;

    if (num == 0) {
        buffer[i++] = '0';
        buffer[i] = '\0';
        return;
    }

    // Based on the behavior of itoa()
    if (num < 0 && base == 10) {
        is_negative = true;
        num = -num;
    }

    while (num != 0) {
        int rem = num % base;
        buffer[i++] = (rem > 9) ? (rem - 10) + 'a' : rem + '0';
        num = num / base;
    }

    if (is_negative) {
        buffer[i++] = '-';
    }

    buffer[i] = '\0';

    str_reverse(buffer, i);

    return;
}


template <u32 length> StringBuffer<length> to_string(int num)
{
    char temp[length];
    arabic__to_string(num, temp, 10);

    return temp;
}



StringBuffer<12> stringify(s32 num)
{
    return to_string<12>(num);
}


// Test harness for non-gba backtesting
class Platform
{

public:
    ~Platform()
    {
        std::ofstream stream("Skyland" + output_,
                             std::ios::out | std::ios::binary);

        for (u8 byte : data_) {
            stream.write((const char*)&byte, 1);
        }
    }

    std::string input_;
    std::string output_;


    Platform(const std::string& input, const std::string& output)
        : input_(input), output_(output)
    {
        std::ifstream stream("Skyland" + input_,
                             std::ios::in | std::ios::binary);
        std::vector<uint8_t> contents((std::istreambuf_iterator<char>(stream)),
                                      std::istreambuf_iterator<char>());
        data_ = contents;

        // std::cout << "loaded data, size: " << data_.size() << std::endl;
    }


    void erase_save_sector()
    {
        auto cap = save_capacity();
        data_.clear();
        for (int i = 0; i < cap; ++i) {
            data_.push_back(0xff);
        }
    }


    void insert_save_byte(u32 offset)
    {
        auto iter = data_.begin() + offset;
        data_.insert(iter, 0xff);
    }


    bool write_save_data(const void* data, u32 data_length, u32 offset)
    {
        if (offset % 2 not_eq 0 or data_length % 2 not_eq 0) {
            std::cout << "write size " << data_length << std::endl;
            std::cout << "bad flash write alignment" << std::endl;
        }

        for (u32 i = 0; i < data_length; ++i) {
            if (data_[offset + i] not_eq 0xff) {
                std::cout << "bad flash write to previously-written mem"
                          << std::endl;
            }
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
#endif // __TEST__



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



static const u8 crc8_table[] = {
    0,   49,  98,  83,  196, 245, 166, 151, 185, 136, 219, 234, 125, 76,  31,
    46,  67,  114, 33,  16,  135, 182, 229, 212, 250, 203, 152, 169, 62,  15,
    92,  109, 134, 183, 228, 213, 66,  115, 32,  17,  63,  14,  93,  108, 251,
    202, 153, 168, 197, 244, 167, 150, 1,   48,  99,  82,  124, 77,  30,  47,
    184, 137, 218, 235, 61,  12,  95,  110, 249, 200, 155, 170, 132, 181, 230,
    215, 64,  113, 34,  19,  126, 79,  28,  45,  186, 139, 216, 233, 199, 246,
    165, 148, 3,   50,  97,  80,  187, 138, 217, 232, 127, 78,  29,  44,  2,
    51,  96,  81,  198, 247, 164, 149, 248, 201, 154, 171, 60,  13,  94,  111,
    65,  112, 35,  18,  133, 180, 231, 214, 122, 75,  24,  41,  190, 143, 220,
    237, 195, 242, 161, 144, 7,   54,  101, 84,  57,  8,   91,  106, 253, 204,
    159, 174, 128, 177, 226, 211, 68,  117, 38,  23,  252, 205, 158, 175, 56,
    9,   90,  107, 69,  116, 39,  22,  129, 176, 227, 210, 191, 142, 221, 236,
    123, 74,  25,  40,  6,   55,  100, 85,  194, 243, 160, 145, 71,  118, 37,
    20,  131, 178, 225, 208, 254, 207, 156, 173, 58,  11,  88,  105, 4,   53,
    102, 87,  192, 241, 162, 147, 189, 140, 223, 238, 121, 72,  27,  42,  193,
    240, 163, 146, 5,   52,  103, 86,  120, 73,  26,  43,  188, 141, 222, 239,
    130, 179, 224, 209, 70,  119, 36,  21,  59,  10,  89,  104, 255, 206, 157,
    172};



struct Root
{
    static constexpr const char* magic_val = "_FS3_LOG";

    u8 magic_[8];
};



struct Record
{
    // NOTE: u16 because our flash chip writes in halfwords.
    enum InvalidateStatus : u16 {
        // A flash erase will set all values in a sector to 0xffff. This is
        // simply how flash storage works. A flash write can only change bits
        // from one to zero, an erase operation sets all bits in a flash sector
        // to one.
        valid = 0xffff,
        invalid = 0x0000
    };

    // NOTE: Don't write this field until you want to invalidate an entry. I
    // mean, no value can be copied to the address of this value, not even
    // InvalidateStatus::valid. On some flash chips, you get one write per
    // address, then the system locks up if you attempt to overwrite anything.
    HostInteger<u16> invalidate_;

    struct FileInfo
    {
        // Crc of the file data. Maybe I should have included the FileInfo in
        // the crc as well. But the file info contains the crc, so I skipped it
        // at the time.
        u8 crc_;

        enum Flags0 {
            // Pad the end of the record to bring it's byte count up to an even
            // size, thus aligning the next record at a halfword boundary.
            has_end_padding = (1 << 0),

            // The file data is compressed. Needs to be decompressed when read.
            compressed = (1 << 1),
        };

        u8 flags_[2];

        u8 name_length_;
        host_u16 data_length_;

        // NOTE: appended data:
        //
        // char name_[name_length_];
        // char padding_[0 or 1];
        // char data_[data_length_];
        // char padding_[0 or 1];
    } file_info_;

    static_assert(sizeof(invalidate_) % 2 == 0 and sizeof(FileInfo) % 2 == 0,
                  "Flash writes require halfword granularity. Struct must be "
                  "neatly halfword copyable.");


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
    ret.bytes_used_ = sector_used() - gap_space;
    ret.bytes_available_ = sector_avail(pfrm) + gap_space;

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



static void compact(Platform& pfrm);



InitStatus initialize(Platform& pfrm, u32 offset)
{
    if (offset % 2 not_eq 0) {
        return failed;
    }

    if (pfrm.save_capacity() == 0) {
        return initialized;
    }

    start_offset = offset;
    auto root = load_root(pfrm);

    if (memcmp(root.magic_, Root::magic_val, 8) not_eq 0) {
        pfrm.erase_save_sector();

        init_root(pfrm, root);

        end_offset = start_offset + sizeof root;

        __path_cache_create(pfrm);

        return initialized;
    }

    info(pfrm, "flash fs found root...");

    offset += sizeof(Root);

    bool reformat = false;


    while (true) {

        if (offset % 2 not_eq 0) {
            info(pfrm, "warning: bad filesystem alignment!");
        }

        Record r;
        pfrm.read_save_data(&r, sizeof r, offset);

        if (r.file_info_.name_length_ == 0xff) {
            // Uninitialized, as it holds the default flash erase value.
            // Uninitialzied name length indicates an unwritten record, i.e. the
            // end of the sector.
            break;
        }

        u8 crc8 = 0;
        int read_size = r.file_info_.data_length_.get();

        for (int i = 0; i < read_size; ++i) {
            u8 val;
            const u32 off = i + offset + (sizeof r) + r.file_info_.name_length_;
            pfrm.read_save_data(&val, 1, off);
            crc8 = crc8_table[((u8)val) ^ crc8];
        }

        if (crc8 not_eq r.file_info_.crc_) {
            // A record has an invalid crc. We'll try to reformat the filesystem
            // and maybe it'll be fixed. I don't know how this can even happen
            // in the first place.
            // NOTE: The filesystem library should not corrupt memory in this
            // way. But the filesystem data blob should be considered an
            // external input to the program and checked for all classes of
            // errors.
            info(pfrm,
                 format(
                     "bad crc! expected: %, got: %", r.file_info_.crc_, crc8));
            reformat = true;
            break;
        }

        if (r.invalidate_.get() not_eq Record::InvalidateStatus::valid) {
            gap_space += r.full_size();
        }

        offset += r.full_size();
    }
    end_offset = offset;

    // Now... we want to scan the rest of the unused portion of the flash
    // filesystem. If any byte is not 0xff, the bit must have been flipped
    // somehow, by, idk, cosmic radiation or something. A successive write to an
    // address in some flash controllers will brick the system, so we want to
    // erase and rewrite the sector in this case.
    for (int i = end_offset; i < pfrm.save_capacity(); ++i) {
        u8 val = 0;
        pfrm.read_save_data(&val, 1, i);
        if (val not_eq 0xff) {
            info(pfrm, "trailing bits unexpectedly flipped!?");
            reformat = true;
            break;
        }
    }

    if (reformat) {
        compact(pfrm);
    }

    __path_cache_create(pfrm);

    info(pfrm,
         format("flash fs init: begin: %, end: %, gaps: %",
                start_offset,
                end_offset,
                gap_space));

    return already_initialized;
}



void walk(Platform& pfrm,
          Function<8 * sizeof(void*), void(const char*)> callback)
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

        char file_name[256];
        memset(file_name, 0, 256);

        pfrm.read_save_data(&file_name, r.file_info_.name_length_, offset);


        if (r.invalidate_.get() == Record::InvalidateStatus::valid) {
            callback(file_name);
        } else {
#ifdef __TEST__
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

        char file_name[256];
        memset(file_name, 0, 256);

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

        if (r.file_info_.name_length_ == 0xff or offset >= end_offset) {
            // uninitialized, as it holds the default flash erase value.
            break;
        }

        offset += sizeof r;

        char file_name[256];
        memset(file_name, 0, 256);

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

    u32 write_offset = start_align;
    Buffer<u8, 64> buffer;

    auto flush = [&] {
        if (buffer.empty()) {
            return;
        }
        pfrm.write_save_data(buffer.data(), buffer.size(), write_offset);
        write_offset += buffer.size();
        buffer.clear();
    };


    for (u32 i = 0; i < data.size(); ++i) {
        if (i == breaks[0]) {
            flush();
            // Bump the write offset past the invalid designator bytes in the
            // record header.
            write_offset += 2;
            breaks.erase(breaks.begin());
            ++i; // Skip the next byte too.
            static_assert(sizeof(Record) == sizeof(Record::FileInfo) + 2);
        } else {
            auto val = data[i];
            if (buffer.full()) {
                flush();
            }
            buffer.push_back(val);
        }
    }

    flush();

    end_offset = write_offset;
    gap_space = 0;

    Root root;
    init_root(pfrm, root);

    info(pfrm, "flash fs completed compaction!");
}



static int batch_write(Platform& pfrm,
                       u32& offset,
                       Vector<char>::Iterator begin,
                       Vector<char>::Iterator end)
{
    constexpr int batch_size = 64;

    Buffer<u8, batch_size> queue;

    int errors = 0;

    auto flush = [&] {
        if (queue.empty()) {
            return;
        }
        bool success = pfrm.write_save_data(queue.data(), queue.size(), offset);

        if (not success) {
            ++errors;
        }

        offset += queue.size();
        queue.clear();
    };

    while (begin not_eq end) {
        if (queue.full()) {
            flush();
        }

        queue.push_back(*begin);
        ++begin;
    }

    flush();

    return errors;
}



bool store_file_data(Platform& pfrm,
                     const char* path,
                     Vector<char>& file_data,
                     const StorageOptions& opts)
{
    // Append a new file to the end of the filesystem log.

    Vector<char> comp_buffer;
    if (opts.use_compression_) {
        compress(file_data, comp_buffer);
    }

    const bool compress_file =
        opts.use_compression_ and
        // FIXME: decompression code mysteriously doesn't work if the input
        // buffer is larger than 2k...
        file_data.chunks_used() < 2;

    auto& input = compress_file ? comp_buffer : file_data;

    bool input_padding = false;
    if (input.size() % 2 not_eq 0) {
        // On the gameboy advance, commodity flash carts can be written only in
        // halfwords (two bytes), so we need to pad the data size to a multiple
        // of two, to ensure that all data has two-byte alignment.
        input.push_back(0);
        input_padding = true;
    }


    auto path_len = str_len(path);
    u8 path_padding = 0;
    if (path_len % 2 not_eq 0) {
        // Add an extra null byte to the end, to bring total size up to a
        // multiple of two.
        path_padding = 1;
    }

    const auto path_total = path_len + path_padding;

    const u32 required_space = input.size() + path_total + sizeof(Record);
    const auto avail_space = sector_avail(pfrm) - sizeof(Record);

    auto existing_size = file_size(pfrm, path);
    // The file already exists. We will unlink it, allowing us to count the
    // existing size toward the available space.
    if (existing_size) {
        existing_size += sizeof(Record) + path_total;
    }

    const bool insufficient_space_remaining = (required_space >= avail_space);
    const bool sufficient_space_after_defrag =
        avail_space + gap_space + existing_size > required_space;

    if (insufficient_space_remaining and sufficient_space_after_defrag) {
        // We can reclaim enough space to store the file by compacting the
        // storage data to squeeze out gaps.

        // We counted the size of the file that we're overwriting toward the
        // available space total. So we have to unlink it.
        unlink_file(pfrm, path);

        compact(pfrm);
    } else if (required_space >= avail_space) {
        // NOTE: don't unlink the existing file, we don't have enough space to
        // store the replacement.
        if (input_padding) {
            input.pop_back();
        }
        return false;
    }

    unlink_file(pfrm, path);

    u8 crc8 = 0;
    for (char c : input) {
        crc8 = crc8_table[((u8)c) ^ crc8];
    }

    // info(pfrm, format("calculated crc %", crc8));

    auto off = end_offset;
    static_assert(sizeof(Record) == sizeof(Record::FileInfo) + 2);
    ++off; // Skip the invalid bytes, which we don't want to write yet.
    ++off;


    Record::FileInfo info;
    info.crc_ = crc8;
    info.name_length_ = path_total;
    info.data_length_.set(input.size());

    info.flags_[0] = 0;
    info.flags_[1] = 0;

    if (input_padding) {
        info.flags_[0] |= Record::FileInfo::Flags0::has_end_padding;
    }
    if (compress_file) {
        info.flags_[0] |= Record::FileInfo::Flags0::compressed;
    }

    int write_errors = 0;

    if (not pfrm.write_save_data(&info, sizeof info, off)) {
        ++write_errors;
    }
    off += sizeof info;

    char file_name[256];
    memset(file_name, 0, 256);
    memcpy(file_name, path, path_len);

    if (not pfrm.write_save_data(file_name, path_total, off)) {
        ++write_errors;
    }
    off += path_total;


    write_errors += batch_write(pfrm, off, input.begin(), input.end());

    end_offset = off;

    __path_cache_insert(path);

    if (input_padding) {
        input.pop_back();
    }

    if (write_errors) {
        // NOTE: write errors indicate that a simultaneous writeback to flash
        // did not work correctly. The data is still stored correctly in SRAM,
        // so future calls to Platform::read_save_data() will work correctly.
        // However, we do want the data to be persisted correctly in flash, so
        // let's try running a compaction operation, which erases the flash chip
        // and writes sram contents back to the flash device. Hopefully doing
        // this will free up any stuck bits.

        ::info(pfrm, "bad flash checksum detected, rewriting sector...");

        compact(pfrm);
    }

    return true;
}



u32 file_size(Platform& pfrm, const char* path)
{
    if (not __path_cache_file_exists_maybe(path)) {
        return 0;
    }

    Record r;

    auto offset = find_file(pfrm, path, r);
    if (offset == -1) {
        return 0;
    }

    return r.file_info_.data_length_.get();
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

    if (r.file_info_.flags_[0] & Record::FileInfo::Flags0::has_end_padding) {
        output.pop_back();
    }

    if (r.file_info_.flags_[0] & Record::FileInfo::Flags0::compressed) {
        Vector<char> decomp;
        decompress(output, decomp);
        output.clear();
        for (char c : decomp) {
            output.push_back(c);
        }
    }

    return output.size();
}



} // namespace flash_filesystem



#ifdef __TEST__



namespace flash_filesystem
{



void reset()
{
    start_offset = 0;
    end_offset = 0;
    gap_space = 0;
}



bool basic_readwrite()
{
    Platform pfrm(".regr_input", ".regr_output");
    initialize(pfrm, 8);

    Vector<char> v1;
    for (int i = 0; i < 21; ++i) {
        v1.push_back('a');
    }

    flash_filesystem::store_file_data(pfrm, "/tmp/rwtest.dat", v1);

    Vector<char> v2;

    flash_filesystem::read_file_data(pfrm, "/tmp/rwtest.dat", v2);

    if (v1.size() not_eq v2.size()) {
        return false;
    }

    for (u32 i = 0; i < v1.size(); ++i) {
        if (v1[i] not_eq v2[i]) {
            return false;
        }
    }

    return true;
}



bool persistence()
{
    Vector<char> v1;
    for (int i = 0; i < 21; ++i) {
        v1.push_back('a');
    }

    Vector<char> v2;
    for (int i = 0; i < 20; ++i) {
        v1.push_back('b');
    }

    {
        Platform pfrm(".regr_input", ".regr_output");
        initialize(pfrm, 8);

        flash_filesystem::store_file_data(pfrm, "/tmp/ptest.dat", v1);
        flash_filesystem::store_file_data(pfrm, "/tmp/ptest2.dat", v2);
    }

    {
        reset();
        Platform pfrm(".regr_output", ".regr_output2");
        initialize(pfrm, 8);

        Vector<char> v1_1;
        Vector<char> v2_1;

        flash_filesystem::read_file_data(pfrm, "/tmp/ptest.dat", v1_1);
        flash_filesystem::read_file_data(pfrm, "/tmp/ptest2.dat", v2_1);

        if (v1_1.size() not_eq v1.size() or v2_1.size() not_eq v2.size()) {
            return false;
        }

        for (u32 i = 0; i < v1.size(); ++i) {
            if (v1_1[i] not_eq v1[i]) {
                return false;
            }
        }

        for (u32 i = 0; i < v2.size(); ++i) {
            if (v2_1[i] not_eq v2[i]) {
                return false;
            }
        }
    }

    return true;
}



bool compaction()
{
    Buffer<std::pair<StringBuffer<68>, Vector<char>>, 9> files;

    {
        Platform pfrm(".regr_input", ".regr_output");
        initialize(pfrm, 8);

        walk(pfrm, [&files, &pfrm](const char* path) {
            Vector<char> file_data;
            read_file_data(pfrm, path, file_data);
            files.push_back({path, file_data});
        });

        compact(pfrm);

        for (auto& kvp : files) {
            Vector<char> data;
            read_file_data(pfrm, kvp.first.c_str(), data);

            if (data.size() not_eq kvp.second.size()) {
                return false;
            }

            for (u32 i = 0; i < data.size(); ++i) {
                if (data[i] not_eq kvp.second[i]) {
                    return false;
                }
            }
        }
    }

    // Persistence test: make sure everything comes up ok after a restart
    // following compaction.
    reset();
    Platform pfrm(".regr_output", ".regr_output2");
    initialize(pfrm, 8);

    if (end_offset not_eq 5396) {
        return false;
    }

    if (gap_space not_eq 0) {
        return false;
    }

    for (auto& kvp : files) {
        Vector<char> data;
        read_file_data(pfrm, kvp.first.c_str(), data);

        if (data.size() not_eq kvp.second.size()) {
            return false;
        }

        for (u32 i = 0; i < data.size(); ++i) {
            if (data[i] not_eq kvp.second[i]) {
                return false;
            }
        }
    }

    return true;
}



bool write_triggered_compaction()
{
    Buffer<std::pair<StringBuffer<68>, Vector<char>>, 9> files;

    Vector<char> test;
    for (int i = 0; i < 9999; ++i) {
        test.push_back('A');
    }

    {
        Platform pfrm(".regr_input", ".regr_output");
        initialize(pfrm, 8);

        walk(pfrm, [&files, &pfrm](const char* path) {
            Vector<char> file_data;
            read_file_data(pfrm, path, file_data);
            files.push_back({path, file_data});
        });

        // trigger compaction
        store_file_data(pfrm, "/stuff.dat", test);

        for (auto& kvp : files) {
            Vector<char> data;
            read_file_data(pfrm, kvp.first.c_str(), data);

            if (data.size() not_eq kvp.second.size()) {
                return false;
            }

            for (u32 i = 0; i < data.size(); ++i) {
                if (data[i] not_eq kvp.second[i]) {
                    return false;
                }
            }
        }
    }

    // Persistence test: make sure everything comes up ok after a restart
    // following compaction.
    reset();
    Platform pfrm(".regr_output", ".regr_output2");
    initialize(pfrm, 8);

    if (end_offset not_eq 15414) {
        return false;
    }

    if (gap_space not_eq 0) {
        return false;
    }

    for (auto& kvp : files) {
        Vector<char> data;
        read_file_data(pfrm, kvp.first.c_str(), data);

        if (data.size() not_eq kvp.second.size()) {
            return false;
        }

        for (u32 i = 0; i < data.size(); ++i) {
            if (data[i] not_eq kvp.second[i]) {
                return false;
            }
        }
    }

    return true;
}



void regression()
{
    int pass_count = 0;
    int fail_count = 0;

    std::cout << "starting fs regression...\n" << std::endl;

#define TEST_CASE(PROC)                                                        \
    reset();                                                                   \
    std::cout << "running: " #PROC "... \n";                                   \
    if (not PROC()) {                                                          \
        puts("[failed]");                                                      \
        ++fail_count;                                                          \
    } else {                                                                   \
        puts("[passed]");                                                      \
        ++pass_count;                                                          \
    }                                                                          \
    puts("");

    TEST_CASE(basic_readwrite);
    TEST_CASE(persistence);
    TEST_CASE(compaction);
    TEST_CASE(write_triggered_compaction);

    puts("");
    std::cout << pass_count << " tests passed" << std::endl;
    std::cout << fail_count << " tests failed" << std::endl;
}



} // namespace flash_filesystem



int main()
{
    flash_filesystem::regression();

    return 0;

    Platform pfrm(".logstructured.sav", ".logstructured.edit.sav");

    flash_filesystem::initialize(pfrm, 8);

    std::cout << "initial walk: " << std::endl;
    flash_filesystem::walk(
        pfrm, [](const char* name) { std::cout << name << std::endl; });


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
    for (int i = 0; i < 999; ++i) {
        test.push_back('a');
    }

    flash_filesystem::store_file_data(pfrm, "/test.dat", test);
    flash_filesystem::store_file_data(pfrm, "/mods/init.lisp", test);

    puts("");

    flash_filesystem::walk(
        pfrm, [](const char* name) { std::cout << name << std::endl; });

    std::cout << "used: " << flash_filesystem::sector_used() << std::endl;
    std::cout << "avail: " << flash_filesystem::sector_avail(pfrm) << std::endl;
    std::cout << "gap: " << flash_filesystem::gap_space << std::endl;



    puts("compact!");
    flash_filesystem::compact(pfrm);
    puts("");

    flash_filesystem::walk(
        pfrm, [](const char* name) { std::cout << name << std::endl; });

    std::cout << "used: " << flash_filesystem::sector_used() << std::endl;
    std::cout << "avail: " << flash_filesystem::sector_avail(pfrm) << std::endl;
    std::cout << "gap: " << flash_filesystem::gap_space << std::endl;

    flash_filesystem::unlink_file(pfrm, "/mods/init.lisp");

    test.clear();
    flash_filesystem::read_file_data(pfrm, "/save/macro.dat", test);
    std::cout << test.size() << std::endl;

    puts("stress test: write a large object repeatedly to trigger compaction:");
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);
    flash_filesystem::store_file_data(pfrm, "/save/macro.dat", test);

    flash_filesystem::walk(
        pfrm, [](const char* name) { std::cout << name << std::endl; });
}
#endif // __TEST__
