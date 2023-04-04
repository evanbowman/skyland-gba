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
