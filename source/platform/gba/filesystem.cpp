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



std::pair<FileContents, FileSize> load(FilePath path)
{
    const char* current = &__rom_end__;
    current += sizeof(Root);

    const auto root = get_root();

    u32 files_remaining = root->file_count_.get();


    while (files_remaining) {
        auto hdr = (FileHeader*)current;

        if (str_eq(hdr->path_, path)) {
            if ((u32)(intptr_t)(current + sizeof(FileHeader)) % 4 not_eq 0) {
                Platform::fatal("unaligned file");
            }
            return {current + sizeof(FileHeader), hdr->size_.get()};
        }

        --files_remaining;
        current += sizeof(FileHeader) + hdr->size_.get();
    }

    return {nullptr, 0};
}


} // namespace filesystem
#pragma GCC diagnostic pop
