////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "flag.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "tile.hpp"



namespace skyland
{



// Generate a wavy animation for the flag, and copy the tiles into vram.
void vram_write_flag(const FlagPixels& px, Layer layer)
{
    u8 tile_data[16][16] = {};


    for (int i = 0; i < FlagPixels::width; ++i) {
        for (int j = 0; j < FlagPixels::height; ++j) {
            tile_data[i][j + 1] = px.pixels[i][j];
        }
    }

    // Flag pole
    for (int j = 0; j < 16; ++j) {
        tile_data[13][j] = 1; // TODO...
    }

    if (layer == Layer::map_0_ext) {
        PLATFORM.overwrite_t0_tile(Tile::flag_start,
                                   PLATFORM.encode_tile(tile_data));
    } else {
        PLATFORM.overwrite_t1_tile(Tile::flag_start,
                                   PLATFORM.encode_tile(tile_data));
    }

    // Now, we want to generate our wavy animation...

    for (int x = 9; x < 12; ++x) { // shift down
        for (int y = 12; y > 0; --y) {
            tile_data[x][y] = tile_data[x][y - 1];
        }
    }

    if (layer == Layer::map_0_ext) {
        PLATFORM.overwrite_t0_tile(Tile::flag_start + 1,
                                   PLATFORM.encode_tile(tile_data));
    } else {
        PLATFORM.overwrite_t1_tile(Tile::flag_start + 1,
                                   PLATFORM.encode_tile(tile_data));
    }


    for (int x = 9; x < 12; ++x) { // undo shift down
        for (int y = 1; y < 13; ++y) {
            tile_data[x][y] = tile_data[x][y + 1];
        }
    }


    for (int x = 5; x < 9; ++x) { // shift down
        for (int y = 12; y > 0; --y) {
            tile_data[x][y] = tile_data[x][y - 1];
        }
    }

    if (layer == Layer::map_0_ext) {
        PLATFORM.overwrite_t0_tile(Tile::flag_start + 2,
                                   PLATFORM.encode_tile(tile_data));
    } else {
        PLATFORM.overwrite_t1_tile(Tile::flag_start + 2,
                                   PLATFORM.encode_tile(tile_data));
    }

    for (int x = 5; x < 9; ++x) { // undo shift down
        for (int y = 1; y < 13; ++y) {
            tile_data[x][y] = tile_data[x][y + 1];
        }
    }

    for (int x = 0; x < 5; ++x) { // shift down
        for (int y = 12; y > 0; --y) {
            tile_data[x][y] = tile_data[x][y - 1];
        }
    }

    if (layer == Layer::map_0_ext) {
        PLATFORM.overwrite_t0_tile(Tile::flag_start + 3,
                                   PLATFORM.encode_tile(tile_data));
    } else {
        PLATFORM.overwrite_t1_tile(Tile::flag_start + 3,
                                   PLATFORM.encode_tile(tile_data));
    }
}



void load_flag(u16 t)
{
    PLATFORM.load_tile0_texture("tilesheet");

    auto data = PLATFORM.extract_tile(Layer::map_0, t);
    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            APP.custom_flag_image_.pixels[x][y] = data.data_[x][y + 1];
        }
    }
}



void FlagPixels::load_custom(Layer layer, u16 offset)
{
    auto data = PLATFORM.extract_tile(layer, 382 + offset);
    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            pixels[x][y] = data.data_[x][y + 1];
        }
    }
}



void load_default_flag()
{
    PLATFORM.load_tile0_texture("tilesheet");

    auto data = PLATFORM.extract_tile(Layer::map_0, Tile::flag_start);
    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            APP.custom_flag_image_.pixels[x][y] = data.data_[x][y + 1];
        }
    }
}



static const char* flag_save_file = "/save/flag.dat";



void FlagPixels::save()
{
    Vector<char> img;
    for (u32 i = 0; i < sizeof pixels; ++i) {
        img.push_back(((u8*)pixels)[i]);
    }

    flash_filesystem::StorageOptions opts{.use_compression_ = true};
    flash_filesystem::store_file_data_binary(flag_save_file, img, opts);
}



void FlagPixels::load()
{
    Vector<char> img;
    if (flash_filesystem::read_file_data_binary(flag_save_file, img)) {
        for (u32 i = 0; i < img.size(); ++i) {
            ((u8*)pixels)[i] = img[i];
        }
    } else {
        load_default_flag();
    }
}



} // namespace skyland
