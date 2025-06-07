////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
            return {{hdr, files_remaining}};
        }

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return nullopt();
}



std::tuple<FileContents, FileSize, const FileHeader*>
load(FilePath path, Optional<DirectoryCache> dir)
{
    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();

    if (dir) {
        current = (const char*)(*dir).first;
        files_remaining = (*dir).second;
    }

    auto path_len = strlen(path);
    if (path_len > sizeof(FileHeader::path_) - 1) {
        PLATFORM.fatal(format("supplied path % exceeds max path len!", path));
    }

    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        if ( // NOTE: because fs paths are padded, we can speed up file search by
            // checking if the last character of the header path matches the
            // last character of the supplied path.
            path[path_len - 1] == hdr->path_[path_len - 1] and
            str_eq(hdr->path_, path)) {

            if ((u32)(intptr_t)(current + sizeof(FileHeader)) % 4 not_eq 0) {
                Platform::fatal("unaligned file");
            }
            return {current + sizeof(FileHeader), hdr->size_.get(), hdr};
        }

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return {nullptr, 0, nullptr};
}


} // namespace filesystem
#pragma GCC diagnostic pop
