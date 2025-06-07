////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "filesystem.hpp"
#include "string.hpp"
#define INCBIN_PREFIX
#define INCBIN_STYLE INCBIN_STYLE_SNAKE
#include "incbin.h"



INCBIN(fs, "fs.bin");



namespace filesystem
{



Root* get_root()
{
    auto root = (Root*)fs_data;
    if (not(root->magic_[0] == '_' and root->magic_[1] == 'F' and
            root->magic_[2] == 'S' and root->magic_[3] == '_')) {
        // The filesystem root must begin with the characters "_FS_". This is
        // how we detect whether there's a filesystem attached to the ROM.
        return nullptr;
    }
    return root;
}



bool is_mounted()
{
    return get_root() not_eq nullptr;
}



void walk(Function<8 * sizeof(void*), void(const char* path)> callback)
{
    const char* current = (const char*)fs_data;
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


FileContents load(FilePath path)
{
    const char* current = (const char*)fs_data;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();


    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        if (str_eq(hdr->path_, path)) {
            return current + sizeof(FileHeader);
        }

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return nullptr;
}



} // namespace filesystem
