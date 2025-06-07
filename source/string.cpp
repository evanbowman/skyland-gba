////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "string.hpp"
#include "platform/platform.hpp"



ScratchBufferPtr save_str(const char* str)
{
    if (strlen(str) - 1 > SCRATCH_BUFFER_SIZE) {
        Platform::fatal("save_str passed excessively long string");
    }
    auto tmp = make_zeroed_sbr("cached-str");
    auto out = tmp->data_;
    while (*str not_eq '\0') {
        *(out++) = *(str++);
    }
    *out = '\0';
    return tmp;
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



char* float_to_string(float f_val, int buffersize, char* result)
{
    int d_val, dec, i;

    f_val += 0.005f;

    d_val = f_val;
    dec = (int)(f_val * 100) % 100;

    memset(result, 0, buffersize);
    result[0] = (dec % 10) + '0';
    result[1] = (dec / 10) + '0';
    result[2] = '.';

    i = 3;
    while (d_val > 0) {
        result[i] = (d_val % 10) + '0';
        d_val /= 10;
        i++;
    }

    str_reverse(result, strlen(result));

    // trim trailing zeroes
    for (int i = strlen(result) - 1; i >= 0; ++i) {
        if (result[i] == '0') {
            result[i] = '\0';
        } else {
            break;
        }
    }

    return result;
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
