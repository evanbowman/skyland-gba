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


#include "flag.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "tile.hpp"



namespace skyland
{



// Generate a wavy animation for the flag, and copy the tiles into vram.
void vram_write_flag(Platform& pfrm, const FlagPixels& px)
{
    u8 tile_data[16][16] = {0};


    for (int i = 0; i < FlagPixels::width; ++i) {
        for (int j = 0; j < FlagPixels::height; ++j) {
            tile_data[i][j + 1] = px.pixels[i][j];
        }
    }

    // Flag pole
    for (int j = 0; j < 16; ++j) {
        tile_data[13][j] = 1; // TODO...
    }

    pfrm.overwrite_t0_tile(Tile::flag_start, pfrm.encode_tile(tile_data));


    // Now, we want to generate our wavy animation...

    for (int x = 9; x < 12; ++x) { // shift down
        for (int y = 12; y > 0; --y) {
            tile_data[x][y] = tile_data[x][y - 1];
        }
    }

    pfrm.overwrite_t0_tile(Tile::flag_start + 1, pfrm.encode_tile(tile_data));


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

    pfrm.overwrite_t0_tile(Tile::flag_start + 2, pfrm.encode_tile(tile_data));



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

    pfrm.overwrite_t0_tile(Tile::flag_start + 3, pfrm.encode_tile(tile_data));
}



void load_default_flag(Platform& pfrm, App& app)
{
    pfrm.load_tile0_texture("tilesheet");

    auto data = pfrm.extract_tile(Layer::map_0, Tile::flag_start);
    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            app.custom_flag_image_.pixels[x][y] = data.data_[x][y + 1];
        }
    }
}



static const char* flag_save_file = "/save/flag.dat";



void FlagPixels::save(Platform& pfrm)
{
    Vector<char> img;
    for (u32 i = 0; i < sizeof pixels; ++i) {
        img.push_back(((u8*)pixels)[i]);
    }

    flash_filesystem::store_file_data_binary(pfrm, flag_save_file, img);
}



void FlagPixels::load(Platform& pfrm, App& app)
{
    Vector<char> img;
    if (flash_filesystem::read_file_data_binary(pfrm, flag_save_file, img)) {
        for (u32 i = 0; i < img.size(); ++i) {
            ((u8*)pixels)[i] = img[i];
        }
    } else {
        load_default_flag(pfrm, app);
    }
}



} // namespace skyland
