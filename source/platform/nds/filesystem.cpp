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
