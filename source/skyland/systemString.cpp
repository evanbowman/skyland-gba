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


#include "systemString.hpp"



namespace skyland
{



// The code below scans the whole file each time in search of a SystemString. We
// speed up the loading by caching the results.
struct IndexCache
{
    using FileOffset = u16;

    FileOffset file_offset_[(int)SystemString::count] = {0};
};



static std::optional<DynamicMemory<IndexCache>> index_cache;



static StringBuffer<32> lang_file;



void systemstring_bind_file(const char* path)
{
    lang_file = path;
    systemstring_drop_index_cache();
}



const char* systemstring_bound_file()
{
    return lang_file.c_str();
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



SystemStringBuffer loadstr(SystemString str)
{
    systemstring_get_index_cache();

    auto result = allocate_dynamic<StringBuffer<1900>>("system-string");

    if (auto data = PLATFORM.load_file_contents("strings", lang_file.c_str())) {
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

        // Scan upwards in the cache until we find a previously-cached line,
        // speeding up lookup in many cases. If we're loading a system string
        // near the end of the file, we can waste a lot of cpu time if we start
        // at the beginning and count newlines.
        int index = target_line - 1;
        while (index > 0) {
            const auto off = (*index_cache)->file_offset_[index];
            if (off) {
                data += off;
                break;
            }
            --index;
        }

        if (index < 0) {
            index = 0;
        }

        while (index not_eq target_line) {
            while (*data not_eq '\n') {
                if (*data == '\0') {
                    PLATFORM.fatal("null byte in localized text");
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
