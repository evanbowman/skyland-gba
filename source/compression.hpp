#pragma once

#include "platform/platform.hpp"
extern "C" {
#include "heatshrink/heatshrink_decoder.h"
#include "heatshrink/heatshrink_encoder.h"
}
#include "memory/buffer.hpp"



template <u32 sz>
Buffer<char, sz> compress(const Buffer<char, sz>& input,
                          Buffer<char, sz>& result)
{
#ifndef HS_COMPRESSOR_OFF
    heatshrink_encoder enc;
    heatshrink_encoder_reset(&enc);


    int write_index = 0;
    while ((u32)write_index < input.size()) {
        size_t remaining = input.size() - write_index;
        size_t bytes_sunk = 0;
        auto sink_res = heatshrink_encoder_sink(
            &enc, (u8*)input.data() + write_index, remaining, &bytes_sunk);

        if (sink_res not_eq HSER_SINK_OK) {
            Platform::fatal("heatshrink invalid api usage");
        }

        write_index += bytes_sunk;

        u8 out_buf[256];
        size_t bytes_read = 0;
        HSE_poll_res res = HSER_POLL_MORE;
        while (res == HSER_POLL_MORE) {
            res = heatshrink_encoder_poll(&enc, out_buf, 256, &bytes_read);

            if (res == HSER_POLL_ERROR_NULL or res == HSER_POLL_ERROR_MISUSE) {
                Platform::fatal("heatshrink api misuse");
            }

            for (u32 i = 0; i < bytes_read; ++i) {
                result.push_back(out_buf[i]);
            }
        }
    }

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

#else
    Platform::fatal("hs encoder disabled!");
#endif

    return result;
}



template <u32 sz> Buffer<char, sz> decompress(const Buffer<char, sz>& input)
{
    heatshrink_decoder enc;
    heatshrink_decoder_reset(&enc);

    Buffer<char, sz> result;

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

    return result;
}
