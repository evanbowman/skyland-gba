////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////



#pragma GCC diagnostic push
// Accessing data past __rom_end__ raises -Warray-bounds errors. These would be
// real errors, except that they aren't problematic on the gameboy advance, for
// various reasons, and we're mounting a filesystem at __rom_end__ anyway, so...
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#pragma GCC diagnostic ignored "-Wstringop-overread"


#include "filesystem.hpp"
#include "platform/platform.hpp"
#include "string.hpp"



// Symbol declared by the linker script, points to the end of the ROM, where we
// will mount the files.
extern char __rom_end__;



namespace filesystem
{



Root* get_root()
{
    auto root = (Root*)&__rom_end__;
    if (not(root->magic_[0] == '_' and root->magic_[1] == 'F' and
            root->magic_[2] == 'S' and root->magic_[3] == '_')) {
        // The filesystem root must begin with the characters "_FS_". This is
        // how we detect whether there's a filesystem attached to the ROM.
        return nullptr;
    }
    return root;
}



u32 size()
{
    if (not is_mounted()) {
        return 0;
    }

    u32 size = sizeof(Root);

    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();


    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        size += sizeof(FileHeader) + hdr->size_.get();

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return size;
}



bool is_mounted()
{
    return get_root() not_eq nullptr;
}



void walk(Function<8 * sizeof(void*), void(const char* path)> callback)
{
    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();


    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        callback(hdr->path_);

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }
}



Optional<DirectoryCache> find_directory(const char* prefix)
{
    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();
    const u32 prefix_len = strlen(prefix);

    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        bool match = true;

        for (u32 i = 0; i < prefix_len; ++i) {
            if (hdr->path_[i] not_eq prefix[i]) {
                match = false;
                break;
            }
        }

        if (match) {
            return root->file_count_.get() - files_remaining;
        }

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return nullopt();
}



std::pair<FileContents, FileSize> load(FilePath path,
                                       Optional<DirectoryCache> dir)
{
    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();
    FileNumber file_no = 0;

    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        if (dir and *dir < file_no) {
            // Keep incrementing until we reach the desired seek position.
        } else if (str_eq(hdr->path_, path)) {
            if ((u32)(intptr_t)(current + sizeof(FileHeader)) % 4 not_eq 0) {
                Platform::fatal("unaligned file");
            }
            return {current + sizeof(FileHeader), hdr->size_.get()};
        }

        --files_remaining;
        ++file_no;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return {nullptr, 0};
}


} // namespace filesystem
#pragma GCC diagnostic pop
