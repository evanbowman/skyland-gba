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


#include "string.hpp"
#include "platform/platform.hpp"



ScratchBufferPtr save_str(const char* str)
{
    if (str_len(str) - 1 > SCRATCH_BUFFER_SIZE) {
        Platform::fatal("save_str passed excessively long string");
    }
    auto tmp = make_scratch_buffer("cached-str");
    auto out = tmp->data_;
    while (*str not_eq '\0') {
        *(out++) = *(str++);
    }
    *out = '\0';
    return tmp;
}



u32 str_len(const char* str)
{
    const char* s;

    for (s = str; *s; ++s)
        ;
    return (s - str);
}



void str_reverse(char str[], int length)
{
    int start = 0;
    int end = length - 1;

    while (start < end) {
        std::swap(*(str + start), *(str + end));
        start++;
        end--;
    }
}



bool str_eq(const char* p1, const char* p2)
{
    while (true) {
        if (*p1 not_eq *p2) {
            return false;
        }
        if (*p1 == '\0' or *p2 == '\0') {
            return true;
        }
        ++p1;
        ++p2;
    }
}



int str_cmp(const char* p1, const char* p2)
{
    const unsigned char* s1 = (const unsigned char*)p1;
    const unsigned char* s2 = (const unsigned char*)p2;

    unsigned char c1, c2;

    do {
        c1 = (unsigned char)*s1++;
        c2 = (unsigned char)*s2++;

        if (c1 == '\0') {
            return c1 - c2;
        }

    } while (c1 == c2);

    return c1 - c2;
}



// In most cases, you do not want to call this function directly, better to call
// the localized version, locale_num2str. Only call arabic__to_string for
// logging purposes, where the language is assumed to be english.
void arabic__to_string(int num, char* buffer, int base);


template <u32 length> StringBuffer<length> to_string(int num)
{
    char temp[length];
    arabic__to_string(num, temp, 10);

    return temp;
}



StringBuffer<12> stringify(s32 num)
{
    return to_string<12>(num);
}
