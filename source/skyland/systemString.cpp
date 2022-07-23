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


#include "systemString.hpp"



namespace skyland
{



// The code below scans the whole file each time in search of a SystemString. We
// speed up the loading by caching the results.
struct IndexCache
{
    using FileOffset = u32;

    FileOffset file_offset_[(int)SystemString::count] = {0};
};



static std::optional<DynamicMemory<IndexCache>> index_cache;



static StringBuffer<32> lang_file;



void systemstring_bind_file(const char* path)
{
    lang_file = path;
    systemstring_drop_index_cache();
}



void systemstring_drop_index_cache()
{
    index_cache.reset();
}



static void systemstring_get_index_cache()
{
    if (not index_cache) {
        index_cache = allocate_dynamic<IndexCache>("locale-string-index-cache");
    }
}



SystemStringBuffer loadstr(Platform& pfrm, SystemString str)
{
    systemstring_get_index_cache();

    auto result = allocate_dynamic<StringBuffer<1900>>("system-string");

    if (auto data = pfrm.load_file_contents("strings", lang_file.c_str())) {
        const char* const data_start = data;

        if ((*index_cache)->file_offset_[(int)str]) {
            data += (*index_cache)->file_offset_[(int)str];

            while (*data not_eq '\0' and *data not_eq '\n') {
                result->push_back(*data);
                ++data;
            }

            return result;
        }

        const int target_line = static_cast<int>(str);

        int index = 0;
        while (index not_eq target_line) {
            while (*data not_eq '\n') {
                if (*data == '\0') {
                    pfrm.fatal("null byte in localized text");
                }
                ++data;
            }
            ++data;

            ++index;
        }

        (*index_cache)->file_offset_[(int)str] = data - data_start;

        while (*data not_eq '\0' and *data not_eq '\n' and *data not_eq '\r') {
            result->push_back(*data);
            ++data;
        }

        return result;

    } else {
        Platform::fatal(
            format("missing language file %", lang_file.c_str()).c_str());
    }
}





} // namespace skyland
