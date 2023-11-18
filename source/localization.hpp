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


#pragma once


#include "allocator.hpp"
#include "dateTime.hpp"
#include "string.hpp"


void locale_set_language(int language_id);
void locale_set_language_english();
int locale_get_language();

bool locale_requires_doublesize_font();

using LocalizedStrBuffer = StringBuffer<1987>;
using LocalizedText = DynamicMemory<LocalizedStrBuffer>;


void set_font_image(const char* font_image_name);


StringBuffer<31> locale_language_name(int language);

LocalizedText locale_localized_language_name(int language);


// string conversion, output as standard arabic numerals (0-9)
void arabic__to_string(int num, char* buffer, int base);


template <u32 length> StringBuffer<length> to_string(int num)
{
    char temp[length];
    arabic__to_string(num, temp, 10);

    return temp;
}


void locale_num2str(int num, char* buffer, int base);


template <u32 buffer_size>
void format_time(StringBuffer<buffer_size>& str, const DateTime& dt)
{
    char buffer[48];

    locale_num2str(dt.date_.month_, buffer, 10);
    str += buffer;
    str += "/";

    locale_num2str(dt.date_.day_, buffer, 10);
    str += buffer;
    str += "/";

    locale_num2str(dt.date_.year_, buffer, 10);
    str += buffer;
    str += " ";

    locale_num2str(dt.hour_, buffer, 10);
    str += buffer;
    str += ":";

    locale_num2str(dt.minute_, buffer, 10);
    str += buffer;
    str += ":";

    locale_num2str(dt.second_, buffer, 10);
    str += buffer;
}


template <u32 buffer_size>
void log_format_time(StringBuffer<buffer_size>& str, const DateTime& dt)
{
    const auto saved_language = locale_get_language();
    locale_set_language(1);

    format_time(str, dt);

    locale_set_language(saved_language);
}
