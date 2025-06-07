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

    auto win = allocate_dynamic<Window>("compr-window");

    heatshrink_encoder enc;
    heatshrink_encoder_reset(&enc);

    for (auto it = input.begin(); it not_eq input.end(); ++it) {
        if (win->input_.full()) {
            compress_sink(enc, win->input_, win->output_);
            for (char c : win->output_) {
                output.push_back(c);
            }
            win->input_.clear();
            win->output_.clear();
        }
        win->input_.push_back(*it);
    }

    if (not win->input_.empty()) {
        compress_sink(enc, win->input_, win->output_);
        for (char c : win->output_) {
            output.push_back(c);
        }
        win->input_.clear();
        win->output_.clear();
    }

    compress_finish(enc, win->output_);
    for (char c : win->output_) {
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

    auto win = allocate_dynamic<Window>("compr-window");

    heatshrink_decoder enc;
    heatshrink_decoder_reset(&enc);

    for (auto it = input.begin(); it not_eq input.end(); ++it) {
        // NOTE: why size == 255? We want to make sure that the output buffer is
        // large enough to hold the decompressed stuff. A greater than 4x
        // compression ratio would be quite high for heatshrink...
        if (win->input_.size() == 255) {
            decompress_sink(enc, win->input_, win->output_);
            for (char c : win->output_) {
                output.push_back(c);
            }
            win->input_.clear();
            win->output_.clear();
        }
        win->input_.push_back(*it);
    }

    if (not win->input_.empty()) {
        decompress_sink(enc, win->input_, win->output_);
        for (char c : win->output_) {
            output.push_back(c);
        }
        win->input_.clear();
        win->output_.clear();
    }

    decompress_finish(enc, win->output_);
    for (char c : win->output_) {
        output.push_back(c);
    }
}
