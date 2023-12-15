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
