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


#include "base32.hpp"



namespace base32
{



static const char* const default_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";



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
