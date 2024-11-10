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



static StringBuffer<32> lang_file;
static bool lang_file_changed = true;
const char* idf_file = nullptr;
const char* idx_file = nullptr;



void systemstring_bind_file(const char* path)
{
    lang_file = path;
    lang_file_changed = true;
}



const char* systemstring_bound_file()
{
    return lang_file.c_str();
}



SystemStringBuffer loadstr(SystemString str)
{
    auto result = allocate_dynamic_fast<StringBuffer<1900>>("system-string");

    if (lang_file_changed) {
        auto path = lang_file;
        auto index = lang_file;
        path += ".idf";
        index += ".idx";
        idf_file = PLATFORM.load_file_contents("strings", path.c_str());
        idx_file = PLATFORM.load_file_contents("strings", index.c_str());

        if (not idf_file or not idx_file) {
            PLATFORM.fatal("system strings file missing!");
        }

        lang_file_changed = false;
    }

    auto data = idf_file;
    auto idx = idx_file;

    if ((int)str == 0) {
        // ...
    } else {
        data += *(((u32*)idx) + ((int)str - 1));
    }

    while (*data not_eq '\0' and *data not_eq '\n' and *data not_eq '\r') {
        result->push_back(*data);
        ++data;
    }

    return result;
}



} // namespace skyland
