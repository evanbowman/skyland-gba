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
