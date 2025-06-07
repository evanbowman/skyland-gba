////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "qr.hpp"
#include "../external/qr/qrcodegen.c"
#include "platform/platform.hpp"



namespace skyland
{



Optional<QRCode> QRCode::create(const char* text)
{
    auto qr_data = make_zeroed_sbr("qrcode-data-buffer");

    auto temp = make_zeroed_sbr("qr-temp-buffer");

    PLATFORM_EXTENSION(watchdog_off);

    bool ok = qrcodegen_encodeText(text,
                                   (u8*)temp->data_,
                                   (u8*)qr_data->data_,
                                   qrcodegen_Ecc_LOW,
                                   qrcodegen_VERSION_MIN,
                                   qrcodegen_VERSION_MAX,
                                   qrcodegen_Mask_AUTO,
                                   true);

    PLATFORM_EXTENSION(watchdog_on);

    if (ok) {
        return QRCode(qr_data);
    } else {
        return {};
    }
}



QRCode::QRCode(ScratchBufferPtr qr_data) : qr_data_(qr_data)
{
}



bool QRCode::get_module(const Vec2<int>& position) const
{
    return qrcodegen_getModule((u8*)qr_data_->data_, position.x, position.y);
}



QRCode::Sidelength QRCode::size() const
{
    return qrcodegen_getSize((u8*)qr_data_->data_);
}



void QRCode::copy_to_vram(u16 tile_start_offset, int format)
{
    auto sz = size();

    // We want to use four pixels to render each QR module.

    int output_tile = tile_start_offset;

    auto is_position_marker_inner = [sz](int x, int y) {
        return (x >= 2 and y >= 2 and x < 5 and y < 5) or
               (x >= 2 and y >= sz - 6 and x < 5 and y < sz - 2) or
               (x > sz - 6 and y < 5 and x < sz - 2 and y > 1);
    };

    auto is_position_marker_outer = [=](int x, int y) {
        return not is_position_marker_inner(x, y) and x < 7 and y < 7;
    };

    if (sz > 76 or format == 1) {
        for (int y = 0; y < sz; y += 8) {
            for (int x = 0; x < sz; x += 8) {
                u8 tile_data[16][16];
                memset(tile_data, 0, sizeof(tile_data));

                for (int yy = 0; yy < 8; ++yy) {
                    for (int xx = 0; xx < 8; ++xx) {
                        if (get_module({y + yy, x + xx})) {
                            u8 color = data_color_;

                            tile_data[xx][yy] = color;
                        }
                    }
                }

                PLATFORM.overwrite_overlay_tile(
                    output_tile++, PLATFORM.encode_tile(tile_data));
            }
        }
    } else if (sz > 36 or format == 2) {
        for (int y = 0; y < sz; y += 4) {
            for (int x = 0; x < sz; x += 4) {
                u8 tile_data[16][16];
                memset(tile_data, 0, sizeof(tile_data));

                for (int yy = 0; yy < 4; ++yy) {
                    for (int xx = 0; xx < 4; ++xx) {
                        if (get_module({y + yy, x + xx})) {
                            u8 color = data_color_;

                            // Set bits for 2x2 block.
                            for (int j = 0; j < 2; ++j) {
                                for (int i = 0; i < 2; ++i) {
                                    tile_data[xx * 2 + i][yy * 2 + j] = color;
                                }
                            }
                        }
                    }
                }

                PLATFORM.overwrite_overlay_tile(
                    output_tile++, PLATFORM.encode_tile(tile_data));
            }
        }
    } else {
        // First row of 2x2 QR module blocks:
        for (int y = 0; y < sz; y += 2) {
            for (int x = 0; x < sz; x += 2) {
                u8 tile_data[16][16];
                memset(tile_data, 0, sizeof(tile_data));

                // For each module in the current 2x2 block
                for (int yy = 0; yy < 2; ++yy) {
                    for (int xx = 0; xx < 2; ++xx) {
                        if (get_module({y + yy, x + xx})) {

                            u8 color = data_color_;
                            if (is_position_marker_inner(x, y)) {
                                color = position_marker_inner_color_;
                            } else if (is_position_marker_outer(x, y)) {
                                color = position_marker_outer_color_;
                            }

                            // Set bits for 4x4 block.
                            for (int j = 0; j < 4; ++j) {
                                for (int i = 0; i < 4; ++i) {
                                    tile_data[xx * 4 + i][yy * 4 + j] = color;
                                }
                            }
                        }
                    }
                }

                PLATFORM.overwrite_overlay_tile(
                    output_tile++, PLATFORM.encode_tile(tile_data));
            }
        }
    }
}



int QRCode::drawsize(int format) const
{
    const auto sz = size();
    if (sz > 76 or format == 1) {
        return size() / 8 + (size() % 8 > 0);
    } else if (sz > 36 or format == 2) {
        return size() / 4 + (size() % 4 > 0);
    } else {
        return size() / 2 + size() % 2;
    }
}



void QRCode::draw(const Vec2<u8>& screen_coord, int format)
{
    copy_to_vram(181, format);

    draw_image(181,
               screen_coord.x,
               screen_coord.y,
               drawsize(format),
               drawsize(format),
               Layer::overlay);
}



} // namespace skyland
