////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"
#include "platform.hpp"
#include "string.hpp"
#include <variant>

// This code is kind of junk. I realized that I needed an INI parser, and
// remembered that I'd written one years ago.



class Conf
{
public:
    Conf()
    {
    }

    using Integer = int;
    using StrBuffer = StringBuffer<2000>;
    using String = DynamicMemory<StrBuffer>;
    using Value = std::variant<std::monostate, Integer, String>;

    Value get(const char* file_data, const char* section, const char* key);
    Value get(const char* section, const char* key);

    template <typename T> T expect(const char* section, const char* key)
    {
        auto v = get(section, key);

        if (auto val = std::get_if<T>(&v)) {
            return std::move(*val);
        } else {
            Platform::fatal(
                format("in config ini: missing % from [%]", key, section)
                    .c_str());
        }
    }
};
