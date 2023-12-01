////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


#pragma once

#include "containers/vector.hpp"
#ifndef __TEST__
extern "C" {
#include "heatshrink/heatshrink_decoder.h"
#include "heatshrink/heatshrink_encoder.h"
}
#else
#include "heatshrink/heatshrink_decoder.h"
#include "heatshrink/heatshrink_encoder.h"
#endif
#include "memory/buffer.hpp"



void compress(const Vector<char>& input, Vector<char>& output);
void decompress(const Vector<char>& input, Vector<char>& output);



template <u32 size>
void compress_sink(heatshrink_encoder& enc,
                   const Buffer<char, size>& input,
                   Buffer<char, size>& result)
{
    int write_index = 0;
    while ((u32)write_index < input.size()) {
        size_t remaining = input.size() - write_index;
        size_t bytes_sunk = 0;
        auto sink_res = heatshrink_encoder_sink(
            &enc, (u8*)input.data() + write_index, remaining, &bytes_sunk);

        if (sink_res not_eq HSER_SINK_OK) {
            // Platform::fatal("heatshrink invalid api usage");
        }

        write_index += bytes_sunk;

        u8 out_buf[256];
        size_t bytes_read = 0;
        HSE_poll_res res = HSER_POLL_MORE;
        while (res == HSER_POLL_MORE) {
            res = heatshrink_encoder_poll(&enc, out_buf, 256, &bytes_read);

            if (res == HSER_POLL_ERROR_NULL or res == HSER_POLL_ERROR_MISUSE) {
                // Platform::fatal("heatshrink api misuse");
            }

            for (u32 i = 0; i < bytes_read; ++i) {
                result.push_back(out_buf[i]);
            }
        }
    }
}


template <u32 size>
void compress_finish(heatshrink_encoder& enc, Buffer<char, size>& result)
{
    auto fin = HSER_FINISH_DONE;
    do {
        fin = heatshrink_encoder_finish(&enc);
        if (fin == HSER_FINISH_MORE) {
            u8 out_buf[256];
            size_t bytes_read = 0;
            heatshrink_encoder_poll(&enc, out_buf, 256, &bytes_read);

            for (u32 i = 0; i < bytes_read; ++i) {
                result.push_back(out_buf[i]);
            }
        }
    } while (fin not_eq HSER_FINISH_DONE);
}



template <u32 sz>
void compress(const Buffer<char, sz>& input, Buffer<char, sz>& result)
{
#ifndef HS_COMPRESSOR_OFF
    heatshrink_encoder enc;
    heatshrink_encoder_reset(&enc);

    compress_sink(enc, input, result);
    compress_finish(enc, result);

#else
    Platform::fatal("hs encoder disabled!");
#endif
}



template <u32 size>
void decompress_sink(heatshrink_decoder& enc,
                     const Buffer<char, size>& input,
                     Buffer<char, size>& result)
{
    int write_index = 0;
    while ((u32)write_index < input.size()) {
        size_t remaining = input.size() - write_index;
        size_t bytes_sunk = 0;
        auto sink_res = heatshrink_decoder_sink(
            &enc, (u8*)input.data() + write_index, remaining, &bytes_sunk);

        if (sink_res not_eq HSDR_SINK_OK) {
            Platform::fatal("heatshrink invalid api usage");
        }

        write_index += bytes_sunk;

        u8 out_buf[256];
        size_t bytes_read = 0;
        HSD_poll_res res = HSDR_POLL_MORE;
        while (res == HSDR_POLL_MORE) {
            res = heatshrink_decoder_poll(&enc, out_buf, 256, &bytes_read);

            if (res == HSDR_POLL_ERROR_NULL or res == HSDR_POLL_ERROR_UNKNOWN) {
                Platform::fatal("heatshrink api misuse");
            }

            for (u32 i = 0; i < bytes_read; ++i) {
                result.push_back(out_buf[i]);
            }
        }
    }
}



template <u32 size>
void decompress_finish(heatshrink_decoder& enc, Buffer<char, size>& result)
{
    auto fin = HSDR_FINISH_DONE;
    do {
        fin = heatshrink_decoder_finish(&enc);
        if (fin == HSDR_FINISH_MORE) {
            u8 out_buf[256];
            size_t bytes_read = 0;
            heatshrink_decoder_poll(&enc, out_buf, 256, &bytes_read);

            for (u32 i = 0; i < bytes_read; ++i) {
                result.push_back(out_buf[i]);
            }
        }
    } while (fin not_eq HSDR_FINISH_DONE);
}



template <u32 sz>
void decompress(const Buffer<char, sz>& input, Buffer<char, sz>& result)
{
    heatshrink_decoder enc;
    heatshrink_decoder_reset(&enc);

    decompress_sink(enc, input, result);
    decompress_finish(enc, result);
}
