////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "compression.hpp"
#include "allocator.hpp"
#include "containers/vector.hpp"



void compress(const Vector<char>& input, Vector<char>& output)
{
    struct Window
    {
        Buffer<char, 1000> input_;
        Buffer<char, 1000> output_;
    };

    auto win_in = allocate<Buffer<char, 1000>>("compr-window-in");
    auto win_out = allocate<Buffer<char, 1400>>("compr-window-out");

    heatshrink_encoder enc;
    heatshrink_encoder_reset(&enc);

    for (auto it = input.begin(); it not_eq input.end(); ++it) {
        if (win_in->full()) {
            compress_sink(enc, *win_in, *win_out);
            for (char c : *win_out) {
                output.push_back(c);
            }
            win_in->clear();
            win_out->clear();
        }
        win_in->push_back(*it);
    }

    if (not win_in->empty()) {
        compress_sink(enc, *win_in, *win_out);
        for (char c : *win_out) {
            output.push_back(c);
        }
        win_in->clear();
        win_out->clear();
    }

    compress_finish(enc, *win_out);
    for (char c : *win_out) {
        output.push_back(c);
    }
}



void decompress(const Vector<char>& input, Vector<char>& output)
{
    struct Window
    {
        Buffer<char, 1000> input_;
        Buffer<char, 1000> output_;
    };

    auto win_in = allocate<Buffer<char, 256>>("dcompr-window-in");
    auto win_out = allocate<Buffer<char, 2000>>("dcompr-window-out");

    heatshrink_decoder enc;
    heatshrink_decoder_reset(&enc);

    for (auto it = input.begin(); it not_eq input.end(); ++it) {
        // NOTE: why size == 255? We want to make sure that the output buffer is
        // large enough to hold the decompressed stuff. A greater than 4x
        // compression ratio would be quite high for heatshrink...
        if (win_in->size() == 255) {
            decompress_sink(enc, *win_in, *win_out);
            for (char c : *win_out) {
                output.push_back(c);
            }
            win_in->clear();
            win_out->clear();
        }
        win_in->push_back(*it);
    }

    if (not win_in->empty()) {
        decompress_sink(enc, *win_in, *win_out);
        for (char c : *win_out) {
            output.push_back(c);
        }
        win_in->clear();
        win_out->clear();
    }

    decompress_finish(enc, *win_out);
    for (char c : *win_out) {
        output.push_back(c);
    }
}
