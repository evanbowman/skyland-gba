#include "flag.hpp"
#include "platform/platform.hpp"
#include "tile.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



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

    auto data = pfrm.extract_tile(Layer::map_0, 105);
    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            app.flag_img_.pixels[x][y] = data.data_[x][y + 1];
        }
    }

}



} // namespace skyland
