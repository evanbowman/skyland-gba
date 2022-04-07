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


#include "base32.hpp"



namespace base32
{



struct Cipher
{
    Buffer<u8, 5> data_;
};



Vector<char> encode(Vector<char>& input)
{
    Vector<char> result;

    auto pos = input.begin();

    const char* alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";

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
