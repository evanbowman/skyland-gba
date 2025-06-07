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

#include "bitvector.hpp"
#include "function.hpp"
#include "number/endian.hpp"
#include "number/numeric.hpp"
#include "platform/libc.hpp"
#include "string.hpp"
#include <optional>



namespace utf8
{

using Codepoint = u32;


// This unicode handling could perhaps be improved... anyway, the scan function
// accepts a c-style string of known length, and invokes a callback for each
// utf8 codepoint encountered. Codepoints will be passed to the callback as
// integers, and the integer values are guaranteed to be comparable to the base
// ascii values, i.e.:
//
// scan([](Codepoint cp, const char*, int) { assert(cp == '0'); }, u8"o", 1)).
//
template <typename Callback>
inline bool scan(Callback&& callback, const char* data, size_t len)
{
    size_t index = 0;
    while (index < len) {

        alignas(Codepoint) char raw[5] = {'\0', '\0', '\0', '\0', '\0'};
        Codepoint* cp = (Codepoint*)raw;

        auto do_callback = [&] {
            if (not is_little_endian()) {
                *cp = __bswap_constant_32(*cp);
            }
            return callback(*cp, raw, index);
        };

        const Bitvector<8> parsed(data[index]);
        if (parsed[7] == 0) {
            raw[0] = data[index];
            if (not do_callback()) {
                return true;
            }
            index += 1;
        } else if (parsed[7] == 1 and parsed[6] == 1 and parsed[5] == 0) {
            raw[0] = data[index];
            raw[1] = data[index + 1];
            if (not do_callback()) {
                return true;
            }
            index += 2;
        } else if (parsed[7] == 1 and parsed[6] == 1 and parsed[5] == 1 and
                   parsed[4] == 0) {
            raw[0] = data[index];
            raw[1] = data[index + 1];
            raw[2] = data[index + 2];
            if (not do_callback()) {
                return true;
            }
            index += 3;
        } else if (parsed[7] == 1 and parsed[6] == 1 and parsed[5] == 1 and
                   parsed[4] == 1 and parsed[3] == 0) {
            raw[0] = data[index];
            raw[1] = data[index + 1];
            raw[2] = data[index + 2];
            raw[3] = data[index + 3];
            if (not do_callback()) {
                return true;
            }
            index += 4;
        } else {
            return false;
        }
    }
    return true;
}


// This returns the first utf8 Codepoint in a string, meant mostly as a utility
// for creating codepoint literals from strings. Unfortunately, C++ doesn't
// offer unicode character literals... I guess I could specify them in hex, but
// that's no fun (and not so easy for other people to read).
//
// NOTE: about CharType template:
// C++20 changed the default underlying type of utf8 literals from char to
// char8_t.
template <typename CharType>
inline Codepoint getc(const CharType* data, int* consumed = nullptr)
{
    Optional<Codepoint> front;
    scan(
        [&front, &consumed](const Codepoint& cp, const char* raw, int) {
            if (not front) {
                front.emplace(cp);
                if (consumed) {
                    *consumed = strlen(raw);
                }
            }
            return true;
        },
        reinterpret_cast<const char*>(data),
        strlen(reinterpret_cast<const char*>(data)));

    if (front) {
        return *front;
    } else {
        return '\0';
    }
}


inline size_t len(const char* data)
{
    size_t ret = 0;
    scan(
        [&ret](const Codepoint&, const char*, int) {
            ++ret;
            return true;
        },
        data,
        strlen(data));
    return ret;
}


//
// BufferedStr is a complicated piece of code meant to speed up random access
// into a utf-8 string. Lots of tradeoffs in this implementation... but
// basically, on the Gameboy Advance, we cannot fit a parsed representation of a
// large utf-8 string in memory, so we have this adapter class, which loads
// small chunks of codepoints from the encoded string.
//
class BufferedStr
{
private:
    const char* const str_;

    static constexpr const int index_count_ = 300;
    utf8::Codepoint data_[index_count_];
    int index_start_;
    int str_len_;

    inline void load_chunk(int index)
    {
        int i = 0;
        size_t stop_offset = str_len_;

        utf8::scan(
            [this, &i, &index](const utf8::Codepoint& cp, const char*, int) {
                if (i >= index and i < index + index_count_) {
                    data_[i - index] = cp;
                }
                i++;
                return true;
            },
            str_,
            stop_offset);

        index_start_ = index;
    }

public:
    inline BufferedStr(const char* str, int len)
        : str_(str), index_start_(0), str_len_(len)
    {
        load_chunk(0);
    }

    inline utf8::Codepoint get(int index)
    {
        if (not(index >= index_start_ and
                index < index_start_ + index_count_)) {
            load_chunk(index);
        }

        return data_[index - index_start_];
    }
};


} // namespace utf8
