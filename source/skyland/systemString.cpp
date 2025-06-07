////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    auto result = allocate_dynamic<StringBuffer<1900>>("system-string");

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
