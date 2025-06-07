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



inline char rot13(char ascii_char)
{
    if (ascii_char >= 'A' && ascii_char <= 'Z') {
        ascii_char = 'A' + (ascii_char - 'A' + 13) % 26;
    } else if (ascii_char >= 'a' && ascii_char <= 'z') {
        ascii_char = 'a' + (ascii_char - 'a' + 13) % 26;
    }
    return ascii_char;
}
