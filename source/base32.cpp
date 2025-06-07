////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "base32.hpp"



namespace base32
{



static const char* const default_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";



Vector<char> decode(Vector<char>& input, const char* alphabet)
{
    // TODO...
    PLATFORM.fatal("implement base32 decoding!");
}



struct Cipher
{
    Buffer<u8, 5> data_;
};



Vector<char> encode(Vector<char>& input, const char* alphabet)
{
    Vector<char> result;

    auto pos = input.begin();

    if (alphabet == nullptr) {
        alphabet = default_alphabet;
    }

    while (pos not_eq input.end()) {

        Cipher cipher;

        int pad = 0;

        for (int i = 0; i < 5; ++i) {
            if (pos == input.end()) {
                while (not cipher.data_.full()) {
                    cipher.data_.push_back(0);
                    ++pad;
                }
                break;
            }

            cipher.data_.push_back(*pos);
            ++pos;
        }

        auto enc = [&](u8 val) { result.push_back(alphabet[val]); };

        // P.S.: this is unintelligible... what was I thinking?
        enc(cipher.data_[0] >> 3);
        enc(((cipher.data_[0] & 0b00000111) << 2) | (cipher.data_[1] >> 6));
        enc((cipher.data_[1] & 0b00111110) >> 1);
        enc(((cipher.data_[1] & 0b00000001) << 4) | (cipher.data_[2] >> 4));
        enc(((cipher.data_[2] & 0x0f) << 1) | (cipher.data_[3] >> 7));
        enc((cipher.data_[3] & 0b01111100) >> 2);
        enc(((cipher.data_[3] & 0x03) << 3) | ((cipher.data_[4] & 0xe0) >> 5));
        enc((cipher.data_[4] & 0b00011111));

        if (pad) {
            auto end = result.end();
            --end;

            const int count = [&] {
                switch (pad) {
                default:
                case 1:
                    return 1;
                case 2:
                    return 3;
                case 3:
                    return 4;
                case 4:
                    return 6;
                }
            }();


            for (int i = 0; i < count; ++i) {
                *end = '=';
                --end;
            }
        }
    }

    return result;
}



} // namespace base32
